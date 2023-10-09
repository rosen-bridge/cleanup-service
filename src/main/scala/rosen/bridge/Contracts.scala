package rosen.bridge

import helpers.{Configs, Utils}
import network.Client
import org.ergoplatform.appkit.{ConstantsBuilder, ErgoContract}
import scorex.crypto.hash.Digest32
import scorex.util.encode.{Base16, Base64}

object Contracts {
  private val client = new Client
  private val ergoClient = client.getClient

  lazy val RWTRepo: ErgoContract = generateRWTRepoContract()
  lazy val WatcherPermit: ErgoContract = generateWatcherPermitContract()
  lazy val Commitment: ErgoContract = generateCommitmentContract()
  lazy val EventTrigger: ErgoContract = generateWatcherTriggerEventContract()
  lazy val Fraud: ErgoContract = generateFraudContract()
  lazy val WatcherCollateral: ErgoContract = generateWatcherCollateralContract()
  lazy val Lock: ErgoContract = generateLockContract()

  def getContractScriptHash(contract: ErgoContract): Digest32 = {
    scorex.crypto.hash.Blake2b256(contract.getErgoTree.bytes)
  }

  private def generateRWTRepoContract(): ErgoContract = {
    ergoClient.execute(ctx => {
      val watcherPermitHash = Base64.encode(getContractScriptHash(WatcherPermit))
      val watcherCollateralHash = Base64.encode(getContractScriptHash(WatcherCollateral))
      val RwtRepoScript = Scripts.RwtRepoScript
        .replace("GUARD_NFT", Base64.encode(Base16.decode(Configs.tokens.GuardNFT).get))
        .replace("RSN_TOKEN", Base64.encode(Base16.decode(Configs.tokens.RSN).get))
        .replace("PERMIT_SCRIPT_HASH", watcherPermitHash)
        .replace("WATCHER_COLLATERAL_SCRIPT_HASH", watcherCollateralHash)
      ctx.compileContract(ConstantsBuilder.create().build(), RwtRepoScript)
    })
  }

  private def generateWatcherPermitContract(): ErgoContract = {
    ergoClient.execute(ctx => {
      val commitmentHash = Base64.encode(getContractScriptHash(Commitment))
      val watcherPermitScript = Scripts.WatcherPermitScript
        .replace("REPO_NFT", Base64.encode(Base16.decode(Configs.tokens.RepoNFT).get))
        .replace("COMMITMENT_SCRIPT_HASH", commitmentHash)
      ctx.compileContract(ConstantsBuilder.create().build(), watcherPermitScript)
    })
  }

  private def generateCommitmentContract(): ErgoContract = {
    ergoClient.execute(ctx => {
      val triggerEvent = Base64.encode(getContractScriptHash(EventTrigger))
      val commitmentScript = Scripts.CommitmentScript
        .replace("REPO_NFT", Base64.encode(Base16.decode(Configs.tokens.RepoNFT).get))
        .replace("EVENT_TRIGGER_SCRIPT_HASH", triggerEvent)
      ctx.compileContract(ConstantsBuilder.create().build(), commitmentScript)
    })
  }

  private def generateWatcherTriggerEventContract(): ErgoContract = {
    ergoClient.execute(ctx => {
      val fraud = Base64.encode(getContractScriptHash(Fraud))
      val lock = Base64.encode(getContractScriptHash(Lock))
      val triggerScript = Scripts.EventTriggerScript
        .replace("CLEANUP_NFT", Base64.encode(Base16.decode(Configs.tokens.CleanupNFT).get))
        .replace("LOCK_SCRIPT_HASH", lock)
        .replace("FRAUD_SCRIPT_HASH", fraud)
        .replace("CLEANUP_CONFIRMATION", Configs.cleanupConfirm.toString)
      ctx.compileContract(ConstantsBuilder.create().build(), triggerScript)
    })
  }

  private def generateFraudContract(): ErgoContract = {
    ergoClient.execute(ctx => {
      val fraudScript = Scripts.FraudScript
        .replace("CLEANUP_NFT", Base64.encode(Base16.decode(Configs.tokens.CleanupNFT).get))
        .replace("REPO_NFT", Base64.encode(Base16.decode(Configs.tokens.RepoNFT).get))
      ctx.compileContract(ConstantsBuilder.create().build(), fraudScript)
    })
  }

  private def generateWatcherCollateralContract(): ErgoContract = {
    ergoClient.execute(ctx => {
      val watcherCollateralScript = Scripts.watcherCollateral
        .replace("REPO_NFT", Base64.encode(Base16.decode(Configs.tokens.RepoNFT).get))
      ctx.compileContract(ConstantsBuilder.create().build(), watcherCollateralScript)
    })
  }

  private def generateLockContract(): ErgoContract = {
    ergoClient.execute(ctx => {
      val lockScript = Scripts.lockScript
        .replace("GUARD_NFT", Base64.encode(Base16.decode(Configs.tokens.GuardNFT).get))
      ctx.compileContract(ConstantsBuilder.create().build(), lockScript)
    })
  }
}
