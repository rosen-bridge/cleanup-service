import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import { OutputBox, Request } from '@ergo-raffle/box-lookup';
import * as ergoLib from 'ergo-lib-wasm-nodejs';
import { SlashTx } from '@rosen-bridge/slash-tx';

import { configs } from '../../configs/config';
import { CleanupTxType, RosenContracts } from '../../types';
import { createFraudBoxRequest } from '../../utils/boxLookupUtils';
import {
  findCollateralBoxByWid,
  getWidFromR4Bytes,
  hasToken,
  outputBoxToErgoBox,
  txToOutputs,
} from '../../utils/cleanupUtils';
import { BoxLookupService } from '../boxLookupService';
import { ScannerService } from '../scannerService';

type CleanupInputState = {
  cleanupBox: ergoLib.ErgoBox;
  feeBoxes: ergoLib.ErgoBox[];
};

/**
 * Handles fraud-box to slash transaction flow.
 * This action is stateless across rounds.
 */
export class SlashAction {
  private fraudRequestId?: number;
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
   * Registers the fraud-box request used for slash tx creation.
   */
  register = (): void => {
    if (this.fraudRequestId !== undefined) return;
    this.fraudRequestId = BoxLookupService.getInstance().addRequest(
      createFraudBoxRequest(
        ergoLib.Address.from_base58(this.contracts.addresses.Fraud)
          .to_ergo_tree()
          .to_base16_bytes(),
        undefined,
        [{ tokenId: this.contracts.tokens.RWTId, amount: 1n }],
        this.getConfirmedFraudBoxes,
        this.onFraudBoxSuffice,
      ),
    );
  };

  /**
   * Unregisters the fraud-box request.
   */
  unregister = (): void => {
    BoxLookupService.getInstance().removeRequest(this.fraudRequestId);
    this.fraudRequestId = undefined;
  };

  /**
   * Returns confirmed unspent fraud boxes.
   *
   * @returns Confirmed unspent fraud boxes
   */
  private getConfirmedFraudBoxes: Request['getConfirmedBoxes'] = async () => {
    return ScannerService.getInstance().getUnspentFraudBoxes();
  };

  /**
   * Builds and enqueues slash transactions for covered fraud boxes.
   * Shared inputs are resolved once before looping fraud boxes.
   *
   * @param boxes - Fraud boxes selected by box-lookup
   * @param unspentBoxes - Current round unspent boxes from box-lookup
   * @returns void
   */
  private onFraudBoxSuffice: Request['onSuffice'] = async (
    boxes: OutputBox[],
    unspentBoxes?: OutputBox[],
  ): Promise<void> => {
    this.logger.info(`onFraudBoxSuffice: got ${boxes.length} fraud boxes`);
    if (boxes.length === 0) return;

    const currentRoundUnspent = unspentBoxes ?? [];
    let cleanupState = await this.resolveCleanupInputs(currentRoundUnspent);
    if (!cleanupState) {
      this.logger.warn(
        'cleanup inputs are not available, skipping slash tx build',
      );
      return;
    }
    let repoBox = await this.resolveRepoBox(currentRoundUnspent);
    if (!repoBox) {
      this.logger.warn('repo box is not available, skipping slash tx build');
      return;
    }

    const collateralByWid = new Map<string, OutputBox>();

    for (const fraud of boxes) {
      const wid = getWidFromR4Bytes(outputBoxToErgoBox(fraud));
      let collateralBox =
        collateralByWid.get(wid) ??
        (await this.resolveCollateralBox(currentRoundUnspent, wid));
      if (!collateralBox) {
        this.logger.info(
          `skipping slash tx build for fraud [${fraud.boxId}]: collateral box for wid [${wid}] not found`,
        );
        continue;
      }

      const height = await ScannerService.getInstance().getCurrentHeight();
      SlashTx.init(
        configs.workflow.minBoxValue,
        configs.workflow.txFee,
        this.logger,
      );
      const result = await SlashTx.getInstance()
        .newBuilder()
        .setFraudBox(outputBoxToErgoBox(fraud))
        .setCollateralBox(outputBoxToErgoBox(collateralBox))
        .setRepoBox(outputBoxToErgoBox(repoBox))
        .setCleanupBox(cleanupState.cleanupBox)
        .setCreationHeight(height)
        .setFeeBoxes(cleanupState.feeBoxes)
        .setChangeAddress(this.cleanupAddress)
        .build();

      const signed = await this.signAndEnqueueTx(
        CleanupTxType.slash,
        result.unsignedTx,
        result.inputBoxes,
        height,
      );
      const outputs = txToOutputs(signed);
      cleanupState = this.findCleanupInputs(outputs)!;
      repoBox = this.findRepoBox(outputs)!;
      collateralBox = this.findCollateralBox(outputs, wid) ?? collateralBox;
      collateralByWid.set(wid, collateralBox);
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
   * Resolves repo box.
   * Tries current round unspent boxes first, then scanner confirmed boxes.
   *
   * @param unspentBoxes - Current round unspent boxes
   * @returns Repo box when found
   */
  private resolveRepoBox = async (
    unspentBoxes: OutputBox[],
  ): Promise<OutputBox | undefined> => {
    const fromUnspent = this.findRepoBox(unspentBoxes);
    if (fromUnspent) return fromUnspent;

    const confirmed =
      await ScannerService.getInstance().getUnspentBoxesByAddress(
        this.contracts.addresses.RWTRepo,
      );
    return this.findRepoBox(confirmed);
  };

  /**
   * Resolves collateral box for a watcher ID.
   * Tries current round unspent boxes first, then scanner confirmed boxes.
   *
   * @param unspentBoxes - Current round unspent boxes
   * @param wid - Watcher id
   * @returns Collateral box when found
   */
  private resolveCollateralBox = async (
    unspentBoxes: OutputBox[],
    wid: string,
  ): Promise<OutputBox | undefined> => {
    const fromUnspent = this.findCollateralBox(unspentBoxes, wid);
    if (fromUnspent) return fromUnspent;

    const confirmed =
      await ScannerService.getInstance().getUnspentCollateralBoxes();
    return this.findCollateralBox(confirmed, wid);
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

  /**
   * Finds repo box by RepoNFT token.
   *
   * @param boxes - Candidate boxes
   * @returns Repo box when found
   */
  private findRepoBox = (boxes: OutputBox[]): OutputBox | undefined => {
    return boxes.find((box) =>
      box.assets.some(
        (asset) => asset.tokenId === this.contracts.tokens.RepoNFT,
      ),
    );
  };

  /**
   * Finds collateral box for a watcher ID.
   *
   * @param boxes - Candidate boxes
   * @param wid - Watcher id
   * @returns Collateral box when found
   */
  private findCollateralBox = (
    boxes: OutputBox[],
    wid: string,
  ): OutputBox | undefined => {
    return findCollateralBoxByWid(boxes, this.contracts.tokens.AwcNFT, wid)[0];
  };
}
