package rosen.bridge

import helpers.{Configs, Utils}
import org.ergoplatform.appkit.{ConstantsBuilder, ErgoContract}
import scorex.crypto.hash.Digest32
import scorex.util.encode.{Base16, Base64}

object Contracts {
  lazy val WatcherBank: ErgoContract = generateWatcherBankContract()
  lazy val WatcherLock: ErgoContract = generateWatcherLockContract()
  lazy val WatcherCommitment: ErgoContract = generateWatcherCommitmentContract()
  lazy val WatcherTriggerEvent: ErgoContract = generateWatcherTriggerEventContract()
  lazy val WatcherFraudLock: ErgoContract = generateWatcherFraudLockContract()

  private def getContractScriptHash(contract: ErgoContract): Digest32 = {
    scorex.crypto.hash.Blake2b256(contract.getErgoTree.bytes)
  }


  private def generateWatcherBankContract(): ErgoContract = {
    Configs.ergoClient.execute(ctx => {
      val watcherLockHash = Base64.encode(getContractScriptHash(WatcherLock))
      val sponsorScript = Scripts.WatcherBankScript
        .replace("GUARD_TOKEN", Base64.encode(Base16.decode(Configs.tokens.GuardNFT).get))
        .replace("RSN_TOKEN", Base64.encode(Base16.decode(Configs.tokens.RSN).get))
        .replace("LOCK_SCRIPT_HASH", watcherLockHash)
      val contract = ctx.compileContract(ConstantsBuilder.create().build(), sponsorScript)
      val address = Utils.getContractAddress(contract)
      println(s"Watcher bank address is : \t\t\t$address")
      contract
    })
  }

  private def generateWatcherLockContract(): ErgoContract = {
    Configs.ergoClient.execute(ctx => {
      val commitmentHash = Base64.encode(getContractScriptHash(WatcherCommitment))
      val sponsorScript = Scripts.WatcherLockScript
        .replace("BANK_NFT", Base64.encode(Base16.decode(Configs.tokens.BankNft).get))
        .replace("COMMITMENT_SCRIPT_HASH", commitmentHash)

      val contract = ctx.compileContract(ConstantsBuilder.create().build(), sponsorScript)
      val address = Utils.getContractAddress(contract)
      println(s"Watcher lock address is : \t\t\t$address")
      contract
    })
  }

  private def generateWatcherCommitmentContract(): ErgoContract = {
    Configs.ergoClient.execute(ctx => {
      val sponsorScript = Scripts.WatcherCommitmentScript
        .replace("BANK_NFT", Base64.encode(Base16.decode(Configs.tokens.BankNft).get))

      val contract = ctx.compileContract(ConstantsBuilder.create().build(), sponsorScript)
      val address = Utils.getContractAddress(contract)
      println(s"Watcher commitment address is : \t\t\t$address")
      contract
    })
  }

  private def generateWatcherTriggerEventContract(): ErgoContract = {
    Configs.ergoClient.execute(ctx => {
      val sponsorScript = Scripts.WatcherTriggerEventScript
        .replace("BANK_NFT", Base64.encode(Base16.decode(Configs.tokens.BankNft).get))

      val contract = ctx.compileContract(ConstantsBuilder.create().build(), sponsorScript)
      val address = Utils.getContractAddress(contract)
      println(s"Watcher trigger event address is : \t\t\t$address")
      contract
    })
  }

  private def generateWatcherFraudLockContract(): ErgoContract = {
    Configs.ergoClient.execute(ctx => {
      val sponsorScript = Scripts.WatcherFraudLockScript
        .replace("BANK_NFT", Base64.encode(Base16.decode(Configs.tokens.BankNft).get))

      val contract = ctx.compileContract(ConstantsBuilder.create().build(), sponsorScript)
      val address = Utils.getContractAddress(contract)
      println(s"Watcher fraud address is : \t\t\t$address")
      contract
    })
  }
}
