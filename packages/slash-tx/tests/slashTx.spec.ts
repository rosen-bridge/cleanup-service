import { beforeEach, describe, expect, it } from 'vitest';
import { SlashTx } from '../lib';

describe('SlashTx', () => {
  beforeEach(() => {
    SlashTx['_instance'] = undefined;
  });

  it('throws when instance is not initialized', () => {
    expect(() => SlashTx.getInstance()).toThrowError(
      'SlashTx instance is not initialized yet',
    );
  });

  it('initializes singleton', () => {
    SlashTx.init(1000000n, '1100000');
    const instance = SlashTx.getInstance();
    expect(instance).toBeDefined();
  });
});
