/**
 * Resets a singleton service instance in tests.
 */
export const resetServiceInstance = (serviceClass: object): void => {
  Reflect.set(serviceClass, 'instance', undefined);
};

/**
 * Builds a minimal TxPot entity-like object for tests.
 *
 * @param serializedTx - base64-encoded unsigned/signed tx
 */
export const makeTxPotEntity = (serializedTx: string) => ({
  txId: 'ignored',
  chain: 'ergo',
  txType: 't',
  status: 'SIGNED',
  requiredSign: 0,
  lastCheck: 0,
  lastStatusUpdate: '0',
  failedInSign: false,
  signFailedCount: 0,
  serializedTx,
});


