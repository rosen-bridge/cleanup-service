package rosen.bridge

import helpers.Configs
import network.Client
import org.ergoplatform.appkit.{ConstantsBuilder, ErgoContract}
import scorex.crypto.hash.Digest32
import scorex.util.encode.{Base16, Base64}

object Contracts {
  private val client = new Client
  private val ergoClient = client.getClient

  lazy val WatcherBank: ErgoContract = generateWatcherBankContract()
  lazy val WatcherLock: ErgoContract = generateWatcherLockContract()
  lazy val WatcherCommitment: ErgoContract = generateWatcherCommitmentContract()
  lazy val WatcherTriggerEvent: ErgoContract = generateWatcherTriggerEventContract()
  lazy val WatcherFraudLock: ErgoContract = generateWatcherFraudLockContract()

  def getContractScriptHash(contract: ErgoContract): Digest32 = {
    scorex.crypto.hash.Blake2b256(contract.getErgoTree.bytes)
  }

  private def generateWatcherBankContract(): ErgoContract = {
    ergoClient.execute(ctx => {
      val watcherLockHash = Base64.encode(getContractScriptHash(WatcherLock))
      val watcherBankScript = Scripts.WatcherBankScript
        .replace("GUARD_TOKEN", Base64.encode(Base16.decode(Configs.tokens.GuardNFT).get))
        .replace("LOCK_SCRIPT_HASH", watcherLockHash)
      val contract = ctx.compileContract(ConstantsBuilder.create().build(), watcherBankScript)
      contract
    })
  }

  private def generateWatcherLockContract(): ErgoContract = {
    ergoClient.execute(ctx => {
      val commitmentHash = Base64.encode(getContractScriptHash(WatcherCommitment))
      val watcherLockScript = Scripts.WatcherLockScript
        .replace("BANK_NFT", Base64.encode(Base16.decode(Configs.tokens.BankNft).get))
        .replace("COMMITMENT_SCRIPT_HASH", commitmentHash)

      val contract = ctx.compileContract(ConstantsBuilder.create().build(), watcherLockScript)
      contract
    })
  }

  private def generateWatcherCommitmentContract(): ErgoContract = {
    ergoClient.execute(ctx => {
      val triggerEvent = Base64.encode(getContractScriptHash(WatcherTriggerEvent))
      val commitmentScript = Scripts.WatcherCommitmentScript
        .replace("BANK_NFT", Base64.encode(Base16.decode(Configs.tokens.BankNft).get))
        .replace("TRIGGER_EVENT_SCRIPT_HASH", triggerEvent)

      val contract = ctx.compileContract(ConstantsBuilder.create().build(), commitmentScript)
      contract
    })
  }

  private def generateWatcherTriggerEventContract(): ErgoContract = {
    ergoClient.execute(ctx => {
      val fraud = Base64.encode(getContractScriptHash(WatcherFraudLock))
      val triggerScript = Scripts.WatcherTriggerEventScript
        .replace("CLEANUP_NFT", Base64.encode(Base16.decode(Configs.tokens.CleanupNFT).get))
        .replace("GUARD_NFT", Base64.encode(Base16.decode(Configs.tokens.GuardNFT).get))
        .replace("FRAUD_SCRIPT_HASH", fraud)
        .replace("CLEANUP_CONFIRMATION", Configs.cleanupConfirm.toString)

      val contract = ctx.compileContract(ConstantsBuilder.create().build(), triggerScript)
      contract
    })
  }

  private def generateWatcherFraudLockContract(): ErgoContract = {
    ergoClient.execute(ctx => {
      val fraudScript = Scripts.WatcherFraudLockScript
        .replace("CLEANUP_NFT", Base64.encode(Base16.decode(Configs.tokens.CleanupNFT).get))
        .replace("BANK_NFT", Base64.encode(Base16.decode(Configs.tokens.BankNft).get))

      val contract = ctx.compileContract(ConstantsBuilder.create().build(), fraudScript)
      contract
    })
  }
}
