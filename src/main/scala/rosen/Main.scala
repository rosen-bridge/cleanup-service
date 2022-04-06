package rosen

import helpers.{Configs, Utils}
import network.Explorer
import org.ergoplatform.appkit._
import rosen.bridge.Boxes
import scorex.util.encode.Base16
import special.collection.Coll

import java.nio.ByteBuffer
import scala.collection.JavaConverters._

class Lock {
  var sk = Utils.randBigInt
  var EWRBox: InputBox = null;
  var UTPBox: InputBox = null;
  var commitment: InputBox = null;

  def getUTP(): Array[Byte] = {
    this.UTPBox.getTokens.get(0).getId.getBytes
  }

  def getUTPHex(): String = {
    this.UTPBox.getTokens.get(0).getId.toString
  }

  def getProver(): ErgoProver = {
    Configs.ergoClient.execute(ctx => {
      ctx.newProverBuilder().withDLogSecret(sk.bigInteger).build()
    })
  }
}

object Main {
  val sk = Utils.randBigInt
  var EWRId: String = ""
  var bank: InputBox = null
  var locks: Seq[Lock] = Seq()
  var triggerEvent: InputBox = null

  def getProver(): ErgoProver = {
    Configs.ergoClient.execute(ctx => {
      ctx.newProverBuilder().withDLogSecret(sk.bigInteger).build()
    })
  }

  val explorer = new Explorer
  val Boxes = new Boxes
/*
  def unlockRSN(ctx: BlockchainContext, EWRCount: Long, index: Int): Unit = {
    val lock = locks(index)
    val prover = lock.getProver()
    val box1 = Boxes.createBoxForUser(ctx, prover.getAddress, 1e9.toLong)
    var R4 = bank.getRegisters.get(0).getValue.asInstanceOf[Coll[Coll[Byte]]].toArray.clone()
    val UTPIndex = R4.map(item => item.toArray).indexOf(lock.UTPBox.getTokens.get(0).getId.getBytes)
    var R5 = bank.getRegisters.get(1).getValue.asInstanceOf[Coll[Long]].toArray.clone()
    if (R5(UTPIndex) == EWRCount) {
      R4 = (R4.slice(0, UTPIndex).toSeq ++ R4.slice(UTPIndex + 1, R4.length)).toArray
      R5 = (R5.slice(0, UTPIndex).toSeq ++ R5.slice(UTPIndex + 1, R5.length)).toArray
      locks = locks.slice(0, index) ++ locks.slice(index + 1, locks.length)
    } else {
      R5.update(UTPIndex, R5(UTPIndex) - EWRCount)
    }
    val bankOut = Boxes.createBankBox(
      ctx,
      EWRId,
      Boxes.getTokenCount(EWRId, bank) + EWRCount,
      Boxes.getTokenCount(Configs.tokens.RSN, bank) - EWRCount * 100L,
      R4.toSeq.map(item => item.toArray),
      R5,
      UTPIndex
    )
    var candidates = Seq(bankOut)
    val boxes = Seq(bank, lock.EWRBox, lock.UTPBox, box1)
    val tokens = Boxes.calcTotalErgAndTokens(Seq(lock.EWRBox, lock.UTPBox, box1))
    tokens.update(Configs.tokens.BankNft, 0)
    tokens.update(EWRId, tokens.getOrElse(EWRId, 0L) - lock.EWRBox.getTokens.get(0).getValue)
    if (lock.EWRBox.getTokens.get(0).getValue > EWRCount) {
      val lockedOut = Boxes.createLockBox(ctx, EWRId, Boxes.getTokenCount(EWRId, lock.EWRBox) - EWRCount, lock.UTPBox.getTokens.get(0).getId.getBytes)
      candidates = candidates ++ Seq(lockedOut)
    }
    tokens.update(Configs.tokens.RSN, tokens.getOrElse(Configs.tokens.RSN, 0L) + EWRCount * 100)
    val totalErgIn: Long = tokens.getOrElse("", 0L)
    val userOutBuilder = ctx.newTxBuilder().outBoxBuilder()
      .contract(ctx.newContract(prover.getAddress.asP2PK().script))
      .value(totalErgIn - Configs.fee - bank.getValue)
    tokens.keys.foreach(tokenId => {
      val amount: Long = tokens.getOrElse(tokenId, 0)
      if (amount > 0 && tokenId != "") {
        userOutBuilder.tokens(new ErgoToken(tokenId, amount))
      }
    })
    candidates = candidates ++ Seq(userOutBuilder.build())
    val unlockTxUnsigned = ctx.newTxBuilder().boxesToSpend(boxes.asJava)
      .fee(Configs.fee)
      .outputs(candidates: _*)
      .sendChangeTo(prover.getAddress.getErgoAddress)
      .build()
    val unlockTx = prover.sign(unlockTxUnsigned)
    //    println(unlockTx.toJson(true))
    println(s"unlocked: ${lock.getUTPHex()}")
    bank = unlockTx.getOutputsToSpend.get(0)
    lock.EWRBox = unlockTx.getOutputsToSpend.get(1)
    //    lock.UTPBox = unlockTx.getOutputsToSpend.get(2)
  }

  def mergeFraudToBank(ctx: BlockchainContext, fraud: InputBox): Unit ={
    val UTP = fraud.getRegisters.get(0).getValue.asInstanceOf[Coll[Coll[Byte]]].toArray(0).toArray
    // TODO must merge one EWR to bank.
    println(s"one ${Base16.encode(UTP)} tokens slashed")
  }

  def moveToFraud(ctx: BlockchainContext): Unit ={
    val prover = getProver()
    val UTPs = triggerEvent.getRegisters.get(0).getValue.asInstanceOf[Coll[Coll[Byte]]].toArray.map(item => item.toArray)
    val box1 = Boxes.createBoxForUser(ctx, prover.getAddress, 1e9.toLong)
    val newFraud = UTPs.map(item => {
      Boxes.createFraudBox(ctx, EWRId, item)
    })
    val unsignedTx = ctx.newTxBuilder().boxesToSpend(Seq(triggerEvent, box1).asJava)
      .fee(Configs.fee)
      .sendChangeTo(prover.getAddress.getErgoAddress)
      .outputs(newFraud: _*)
      .build()
    val tx = prover.sign(unsignedTx)
    println("fraud detected")
    triggerEvent = null;
    newFraud.indices.foreach(index => mergeFraudToBank(ctx, tx.getOutputsToSpend.get(index)))
  }
*/
  def main(args: Array[String]): Unit = {
    /*
    val explorer = new Explorer
    val boxes = explorer.getUnspentTokenBoxIds("008502d25e61063d1f3be9829e70590f94dd9089b8d0756c8cafdfbd2a303fb4")
    println(s"boxes:\n$boxes")

    println(s"Code executed successfully!")
    */
    /*
    Configs.ergoClient.execute(ctx => {
      createBankBox(ctx, 1e12.toLong, 100L, "ADA")
      (1 to 9).foreach(item => lockRSN(ctx, 10000L))
      unlockRSN(ctx, 50L, 2)
      unlockRSN(ctx, 50L, 2)
      redeemBank(ctx)
      val commitment = new Commitment()
      locks.indices.foreach(index => createCommitment(ctx, commitment, index))
      redeemCommitment(ctx, 0)
      triggerEvent(ctx, 1, Seq(5), commitment)
      guardPayment(ctx, commitment)
      locks.indices.foreach(index => createCommitment(ctx, commitment, index))
      triggerEvent(ctx, 0, Seq(), commitment)
      moveToFraud(ctx)
    })
    */
  }
}
