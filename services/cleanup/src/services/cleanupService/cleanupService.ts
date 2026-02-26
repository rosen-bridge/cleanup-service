import { AbstractLogger } from '@rosen-bridge/abstract-logger';
import {
  Dependency,
  PeriodicTaskService,
  ServiceStatus,
} from '@rosen-bridge/service-manager';
import { TransactionStatus } from '@rosen-bridge/tx-pot';
import * as ergoLib from 'ergo-lib-wasm-nodejs';

import { configs } from '../../config/config';
import { ERGO_CHAIN_NAME } from '../../config/constants';
import { loadRosenContracts } from '../../config/contractsConfig';
import { CleanupTxType, RosenContracts } from '../../types';
import { mnemonicToAddress, signTx } from '../../utils/ergoUtils';
import { BoxLookupService } from '../boxLookupService';
import { DBService } from '../dbService';
import { ScannerService } from '../scannerService';
import { TxPotService } from '../txPotService';
import { FraudAction } from './fraudAction';
import { SlashAction } from './slashAction';

/**
 * New cleanup service.
 * Handles fraud and slash action flows.
 */
export class CleanupService extends PeriodicTaskService {
  static name = 'CleanupService';
  protected name = CleanupService.name;
  taskName = 'CleanupTask';
  private static instance?: CleanupService;

  protected dependencies: Dependency[] = [
    { serviceName: DBService.name, allowedStatuses: [ServiceStatus.running] },
    {
      serviceName: ScannerService.name,
      allowedStatuses: [ServiceStatus.running],
    },
    {
      serviceName: TxPotService.name,
      allowedStatuses: [ServiceStatus.running],
    },
    {
      serviceName: BoxLookupService.name,
      allowedStatuses: [ServiceStatus.running],
    },
  ];

  private contracts?: RosenContracts;
  private cleanupAddress?: string;
  private fraudAction?: FraudAction;
  private slashAction?: SlashAction;

  private constructor(logger?: AbstractLogger) {
    super(logger);
  }

  /**
   * Initializes the singleton instance.
   */
  static init = (logger?: AbstractLogger) => {
    if (this.instance) return;
    this.instance = new CleanupService(logger);
  };

  /**
   * Returns the singleton instance.
   */
  static getInstance = (): CleanupService => {
    if (!this.instance)
      throw new Error('CleanupService instance is not initialized yet');
    return this.instance;
  };

  /**
   * Initializes runtime values and registers action requests.
   */
  protected preStart = async (): Promise<void> => {
    this.prepareRuntime();
    if (!this.contracts || !this.cleanupAddress) {
      throw new Error('CleanupService is not prepared');
    }

    this.fraudAction = new FraudAction(
      this.logger,
      this.contracts,
      this.cleanupAddress,
      this.signAndEnqueueTx,
    );
    this.slashAction = new SlashAction(
      this.logger,
      this.contracts,
      this.cleanupAddress,
      this.signAndEnqueueTx,
    );
    this.fraudAction.register();
    this.slashAction.register();
  };

  /**
   * Unregisters action requests.
   */
  protected postStop = async (): Promise<void> => {
    this.fraudAction?.unregister();
    this.slashAction?.unregister();
    this.fraudAction = undefined;
    this.slashAction = undefined;
  };

  /**
   * Signs an unsigned tx and adds it to txpot as SIGNED.
   */
  private signAndEnqueueTx = async (
    txType: CleanupTxType,
    unsignedTx: ergoLib.UnsignedTransaction,
    inputBoxes: ergoLib.ErgoBox[],
    height: number,
  ): Promise<ergoLib.Transaction> => {
    const ctx = await TxPotService.getInstance()
      .getErgoNetworkInterface()
      .network.getErgoStateContext();
    const signed = await signTx(
      ctx,
      configs.workflow.cleanupMnemonic,
      unsignedTx,
      inputBoxes,
    );
    const serializedTx = Buffer.from(signed.sigma_serialize_bytes()).toString(
      'base64',
    );
    this.logger.info(
      `signing and enqueuing tx ${signed.id().to_str()} for ${txType} with ${inputBoxes.length} input boxes`,
    );
    await TxPotService.getInstance()
      .getTxPot()
      .addTx(
        signed.id().to_str(),
        ERGO_CHAIN_NAME,
        txType,
        0,
        serializedTx,
        TransactionStatus.SIGNED,
        height,
      );
    return signed;
  };

  /**
   * Loads contracts and derives cleanup address.
   */
  private prepareRuntime = (): void => {
    this.contracts = loadRosenContracts(configs.workflow.contractsPath);
    const prefix =
      configs.workflow.ergoNetwork === 'mainnet'
        ? ergoLib.NetworkPrefix.Mainnet
        : ergoLib.NetworkPrefix.Testnet;
    this.cleanupAddress = mnemonicToAddress(
      configs.workflow.cleanupMnemonic,
      prefix,
    );
  };

  /**
   * Periodic task to keep service alive.
   */
  protected getTasks = () => {
    return [
      {
        fn: this.tick,
        interval: configs.intervals.workflow * 1000,
      },
    ];
  };

  /**
   * No-op periodic task. All work is callback-driven.
   */
  private tick = async (): Promise<void> => {
    return;
  };
}
