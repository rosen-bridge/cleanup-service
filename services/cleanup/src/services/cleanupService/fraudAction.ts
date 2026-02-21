import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import { OutputBox, Request } from '@ergo-raffle/box-lookup';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { FraudTx, TriggerEventData } from '@rosen-bridge/fraud-tx';

import { configs } from '../../configs/config';
import { ScannerService } from '../scannerService';
import { BoxLookupService } from '../boxLookupService';
import { CleanupTxType, RosenContracts } from '../../types';
import { createTriggerEventRequest } from '../../utils/boxLookupUtils';
import {
  outputBoxToErgoBox,
  getCommitmentCountFromR7,
  getWidListDigestFromR4,
  getTokenAmount,
  hasToken,
  txToOutputs,
} from '../../utils/cleanupUtils';

type CleanupInputState = {
  cleanupBox: ergoLib.ErgoBox;
  feeBoxes: ergoLib.ErgoBox[];
};

/**
 * Handles trigger-event to fraud transaction flow.
 * This action is stateless across rounds.
 */
export class FraudAction {
  private triggerRequestId?: number;
  private readonly cleanupErgoTree: string;

  constructor(
    private readonly logger: AbstractLogger,
    private readonly contracts: RosenContracts,
    private readonly cleanupAddress: string,
    private readonly signAndEnqueueTx: (
      txType: CleanupTxType,
      unsignedTx: ergoLib.UnsignedTransaction,
      inputBoxes: ergoLib.ErgoBox[],
      height: number,
    ) => Promise<ergoLib.Transaction>,
  ) {
    this.cleanupErgoTree = ergoLib.Address.from_base58(cleanupAddress)
      .to_ergo_tree()
      .to_base16_bytes();
  }

  /**
   * Registers the trigger-event request used for fraud tx creation.
   */
  register = (): void => {
    if (this.triggerRequestId !== undefined) return;
    this.triggerRequestId = BoxLookupService.getInstance().addRequest(
      createTriggerEventRequest(
        ergoLib.Address.from_base58(
          this.contracts.addresses.WatcherTriggerEvent,
        )
          .to_ergo_tree()
          .to_base16_bytes(),
        undefined,
        [{ tokenId: this.contracts.tokens.RWTId, amount: 1n }],
        this.getConfirmedTriggerBoxes,
        this.onTriggerEventSuffice,
      ),
    );
  };

  /**
   * Unregisters the trigger-event request.
   */
  unregister = (): void => {
    BoxLookupService.getInstance().removeRequest(this.triggerRequestId);
    this.triggerRequestId = undefined;
  };

  /**
   * Returns confirmed trigger boxes that are old enough for cleanup.
   *
   * @returns Confirmed expired trigger boxes
   */
  private getConfirmedTriggerBoxes: Request['getConfirmedBoxes'] = async () => {
    const height = await ScannerService.getInstance().getCurrentHeight();
    const expiredBefore = height - this.contracts.cleanupConfirm;
    const confirmed =
      await ScannerService.getInstance().getUnspentTriggerBoxes();
    return confirmed.filter((box) => box.creationHeight <= expiredBefore);
  };

  /**
   * Builds and enqueues fraud transactions for covered trigger boxes.
   * Initial cleanup and fee inputs are resolved once before looping triggers.
   *
   * @param boxes - Trigger boxes selected by box-lookup
   * @param unspentBoxes - Current round unspent boxes from box-lookup
   * @returns void
   */
  private onTriggerEventSuffice: Request['onSuffice'] = async (
    boxes: OutputBox[],
    unspentBoxes?: OutputBox[],
  ): Promise<void> => {
    this.logger.info(
      `onTriggerEventSuffice: got ${boxes.length} trigger-event boxes`,
    );
    if (boxes.length === 0) return;

    const height = await ScannerService.getInstance().getCurrentHeight();
    let cleanupState = await this.resolveCleanupInputs(unspentBoxes ?? []);
    if (!cleanupState) {
      this.logger.warn(
        'cleanup inputs are not available, skipping fraud tx build',
      );
      return;
    }

    for (const trigger of boxes) {
      if (trigger.creationHeight > height - this.contracts.cleanupConfirm) {
        this.logger.info(
          `skipping fraud tx build for trigger [${trigger.boxId}]: ` +
            `creationHeight (${trigger.creationHeight}) > height (${height}) - cleanupConfirm (${this.contracts.cleanupConfirm})`,
        );
        continue;
      }

      const triggerBox = outputBoxToErgoBox(trigger);
      const digest = getWidListDigestFromR4(triggerBox);
      const commitmentCount = getCommitmentCountFromR7(triggerBox);
      const wids = await ScannerService.getInstance().getTriggerWidsByTxId(
        trigger.transactionId,
      );
      if (wids.length !== commitmentCount) {
        this.logger.warn(
          `Skipping fraud tx build for trigger [${trigger.boxId}]: commitmentCount mismatch for triggerTxId=${trigger.transactionId}. ` +
            `commitmentCount=${commitmentCount}, wids.length=${wids.length}, widDigest=${digest}`,
        );
        continue;
      }

      const triggerData: TriggerEventData = {
        box: triggerBox,
        wids,
        rwtAmount: getTokenAmount(triggerBox, this.contracts.tokens.RWTId),
      };

      FraudTx.init(
        this.contracts.addresses.Fraud,
        this.cleanupAddress,
        this.contracts.tokens.RWTId,
        configs.workflow.minBoxValue,
        configs.workflow.txFee,
        this.logger,
      );
      const result = await FraudTx.getInstance()
        .newBuilder()
        .setTriggerEventData(triggerData)
        .setCleanerBox(cleanupState.cleanupBox)
        .setCreationHeight(height)
        .setFeeBoxes(cleanupState.feeBoxes)
        .setChangeAddress(this.cleanupAddress)
        .build();

      const signed = await this.signAndEnqueueTx(
        CleanupTxType.fraud,
        result.unsignedTx,
        result.inputBoxes,
        height,
      );
      cleanupState = this.findCleanupInputs(txToOutputs(signed))!;
    }
  };

  /**
   * Resolves cleanup box and fee boxes.
   * Tries current round unspent boxes first, then scanner confirmed boxes.
   *
   * @param unspentBoxes - Current round unspent boxes
   * @returns Cleanup input state when found
   */
  private resolveCleanupInputs = async (
    unspentBoxes: OutputBox[],
  ): Promise<CleanupInputState | undefined> => {
    const fromUnspent = this.findCleanupInputs(unspentBoxes);
    if (fromUnspent) return fromUnspent;

    const confirmed =
      await ScannerService.getInstance().getUnspentBoxesByAddress(
        this.cleanupAddress,
      );
    return this.findCleanupInputs(confirmed);
  };

  /**
   * Finds cleanup NFT box and fee boxes among boxes belonging to cleanup address.
   *
   * @param boxes - Candidate boxes
   * @returns Parsed cleanup input state when found
   */
  private findCleanupInputs = (
    boxes: OutputBox[],
  ): CleanupInputState | undefined => {
    const sameAddressBoxes = boxes.filter(
      (box) => box.ergoTree === this.cleanupErgoTree,
    );
    if (sameAddressBoxes.length === 0) return undefined;

    let cleanupBox: ergoLib.ErgoBox | undefined;
    const feeBoxes: ergoLib.ErgoBox[] = [];
    for (const box of sameAddressBoxes) {
      const parsed = outputBoxToErgoBox(box);
      if (!cleanupBox && hasToken(parsed, this.contracts.tokens.CleanupNFT)) {
        cleanupBox = parsed;
      } else {
        feeBoxes.push(parsed);
      }
    }

    if (!cleanupBox) return undefined;
    return { cleanupBox, feeBoxes };
  };
}
