import { beforeEach, describe, expect, it } from 'vitest';
import { FraudTx } from '../lib';

describe('FraudTx', () => {
  beforeEach(() => {
    FraudTx['_instance'] = undefined;
  });

  it('should throw exception when FraudTx._instance is not yet initialized', () => {
    expect(() => FraudTx.getInstance()).toThrowError(
      'FraudTx instance is not initialized yet',
    );
  });

  it('should initialize singleton correctly', () => {
    FraudTx.init(
      'fraud-address',
      'cleaner-address',
      'rwt-id',
      1000000n,
      '1100000',
    );

    const instance = FraudTx.getInstance();
    expect(instance).toBeDefined();
    expect(instance['fraudAddress']).toEqual('fraud-address');
    expect(instance['cleanerAddress']).toEqual('cleaner-address');
    expect(instance['rwtTokenId']).toEqual('rwt-id');
    expect(instance['minBoxValue']).toEqual(1000000n);
    expect(instance['txFee']).toEqual('1100000');
  });

  it('should reject invalid creation height in builder', () => {
    FraudTx.init(
      'fraud-address',
      'cleaner-address',
      'rwt-id',
      1000000n,
      '1100000',
    );
    expect(() =>
      FraudTx.getInstance().newBuilder().setCreationHeight(0),
    ).toThrowError('Creation height must be a positive integer');
  });
});
