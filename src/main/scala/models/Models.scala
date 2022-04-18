package models

import helpers.RosenExceptions.UnexpectedException
import helpers.{Configs, Utils}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{ErgoToken, ErgoType, ErgoValue, InputBox, JavaHelpers, OutBox, UnsignedTransactionBuilder}
import rosen.bridge.Contracts
import special.collection.Coll

import scala.collection.JavaConverters._


class ErgoBox(inBox: InputBox) {

  def getBox: InputBox = inBox

  def getId: String = inBox.getId.toString

  def getTokens: Seq[ErgoToken] = inBox.getTokens.asScala

  def getErgs: Long = inBox.getValue

  /**
   * adds a token to sequence of token (update sequence in case the token already exists)
   * @param tokenSeq sequence of tokens
   * @param boxToken token to be added
   * @return
   */
  protected def addBoxToken(tokenSeq: Seq[ErgoToken], boxToken: ErgoToken): Seq[ErgoToken] = {
    val tokenIds = tokenSeq.map(_.getId.toString)
    if (tokenIds.contains(boxToken.getId.toString)) {
      val tokenIndex = tokenIds.indexOf(boxToken.getId.toString)
      tokenSeq.slice(0, tokenIndex) ++
        tokenSeq.slice(tokenIndex + 1, tokenSeq.length) :+
        new ErgoToken(boxToken.getId.toString, tokenSeq(tokenIndex).getValue + boxToken.getValue)
    }
    else tokenSeq :+ boxToken
  }

}

class TriggerEventBox(eventBox: InputBox) extends ErgoBox(eventBox) {

  /**
   * returns array of watcher UTPs in register R4 of trigger event box
   */
  def getUTPs: Array[Array[Byte]] = eventBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Coll[Byte]]].toArray.map(item => item.toArray)

  /**
   * returns number of watchers (UTPs) in event box
   */
  def getWatchersLen: Int = getUTPs.length

  /**
   * creates watcher fraud lock boxes
   * @param txB transaction builder
   */
  def createFraudBoxes(txB: UnsignedTransactionBuilder): Seq[OutBox] = {
    getUTPs.map(UTP => {
      txB.outBoxBuilder()
        .value(Configs.minBoxValue)
        .contract(Contracts.WatcherFraudLock)
        .tokens(new ErgoToken(Configs.tokens.EWR, 1))
        .registers(ErgoValue.of(Seq(UTP).map(item => JavaHelpers.SigmaDsl.Colls.fromArray(item)).toArray, ErgoType.collType(ErgoType.byteType())))
        .build()
    }).toSeq
  }

}

class CleanerBox(cleanerBox: InputBox) extends ErgoBox(cleanerBox) {

  /**
   * returns true if the box has enough erg to create new cleanerBox and pay tx fee
   * @param feeBoxesErgs the amount of erg in the additional boxes of cleaner
   */
  def hasEnoughErg(feeBoxesErgs: Long = 0): Boolean = cleanerBox.getValue + feeBoxesErgs >= Configs.minBoxValue + Configs.fee

  /**
   * creates new cleaner box using current cleaner box and feeBoxes and size of watchers in fraud trigger event box
   * @param txB transaction builder
   * @param feeBoxes additional boxes of cleaner
   * @return
   */
  def createCleanerBox(txB: UnsignedTransactionBuilder, feeBoxes: Seq[InputBox]): OutBox = {
    var tokensSeq: Seq[ErgoToken] = cleanerBox.getTokens.asScala
    var cleanerTotalErg: Long = cleanerBox.getValue - Configs.fee

    // calculate feeBoxes assets, add to cleaner box assets
    feeBoxes.foreach(feeBox => {
      cleanerTotalErg += feeBox.getValue
      feeBox.getTokens.asScala.foreach(token => tokensSeq = addBoxToken(tokensSeq, token))
    })

    // generate cleaner box
    txB.outBoxBuilder()
      .value(cleanerTotalErg)
      .contract(new ErgoTreeContract(cleanerBox.getErgoTree, Configs.node.networkType))
      .tokens(tokensSeq: _*)
      .build()
  }

}

class FraudBox(fraudBox: InputBox) extends ErgoBox(fraudBox) {

  /**
   * returns watcher UTP in register R4 of fraud box
   */
  def getUTP: Array[Byte] = fraudBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Coll[Byte]]].toArray(0).toArray.clone()

  /**
   * creates slashed box which contains unlocked RSN
   * @param txB transaction builder
   */
  def createSlashedBox(txB: UnsignedTransactionBuilder, RSNAmount: Long): OutBox = {
    txB.outBoxBuilder()
      .value(Configs.minBoxValue)
      .contract(Utils.getAddressContract(Configs.cleaner.slashedAddress))
      .tokens(new ErgoToken(Configs.tokens.RSN, RSNAmount))
      .build()
  }

}

class BankBox(bankBox: InputBox) extends ErgoBox(bankBox) {

  /**
   * returns array of watcher UTPs in register R4 of bank box
   */
  def getUTPs: Array[Array[Byte]] = bankBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Coll[Byte]]].toArray.map(item => item.toArray)

  /**
   * returns the number of EWRs for each UTP in register R5 of bank box
   */
  def getEWRs: Array[Long] = bankBox.getRegisters.get(1).getValue.asInstanceOf[Coll[Long]].toArray.clone()

  /**
   * returns register R6
   */
  def getR6Values: Array[Long] = bankBox.getRegisters.get(2).getValue.asInstanceOf[Coll[Long]].toArray.clone()

  /**
   * returns price of EWR in RSN (first value of register R6)
   */
  def getRSNFactor: Long = getR6Values.head

  /**
   * creates new bank box using current bank box with new UTPs and EWRs passed by as arguments
   * @param txB transaction builder
   * @param UTPs array of watcher UTPs in register R4
   * @param EWRs array of watcher EWRs in register R5
   * @param watcherIndex the slashed UTP index in array of watcher UTPs
   */
  def createBankBox(txB: UnsignedTransactionBuilder, UTPs: Array[Array[Byte]], EWRs: Array[Long], watcherIndex: Int): OutBox = {
    val R4 = UTPs.map(item => JavaHelpers.SigmaDsl.Colls.fromArray(item))
    val R5 = JavaHelpers.SigmaDsl.Colls.fromArray(EWRs)
    val R6 = JavaHelpers.SigmaDsl.Colls.fromArray(getR6Values)

    txB.outBoxBuilder()
      .value(Configs.minBoxValue)
      .tokens(
        new ErgoToken(Configs.tokens.BankNft, 1),
        new ErgoToken(Configs.tokens.EWR, EWRAmount + 1),
        new ErgoToken(Configs.tokens.RSN, RSNAmount - this.getRSNFactor)
      )
      .contract(Contracts.WatcherBank)
      .registers(
        ErgoValue.of(R4, ErgoType.collType(ErgoType.byteType())),
        ErgoValue.of(R5, ErgoType.longType()),
        ErgoValue.of(R6, ErgoType.longType()),
        ErgoValue.of(watcherIndex)
      ).build()
  }

  /**
   * returns the amount EWR in bank box
   */
  private def EWRAmount: Long = bankBox.getTokens.asScala.find(token => token.getId.toString == Configs.tokens.EWR)
    .getOrElse(throw UnexpectedException(s"Token EWR ${Configs.tokens.EWR} not found in bankBox with Id ${bankBox.getId.toString}"))
    .getValue

  /**
   * returns the amount RSN in bank box
   */
  private def RSNAmount: Long = bankBox.getTokens.asScala.find(token => token.getId.toString == Configs.tokens.RSN)
    .getOrElse(throw UnexpectedException(s"Token RSN ${Configs.tokens.RSN} not found in bankBox with Id ${bankBox.getId.toString}"))
    .getValue

}
