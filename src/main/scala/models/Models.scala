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
   * returns array of watcher WIDs in register R4 of trigger event box
   */
  def getWIDs: Array[Array[Byte]] = eventBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Coll[Byte]]].toArray.map(item => item.toArray)

  /**
   * returns number of watchers (WIDs) in event box
   */
  def getWatchersLen: Int = getWIDs.length

  /**
   * creates watcher fraud boxes
   * @param txB transaction builder
   */
  def createFraudBoxes(txB: UnsignedTransactionBuilder): Seq[OutBox] = {
    getWIDs.map(WID => {
      txB.outBoxBuilder()
        .value(Configs.minBoxValue)
        .contract(Contracts.Fraud)
        .tokens(new ErgoToken(Configs.tokens.RWT, 1))
        .registers(ErgoValue.of(Seq(WID).map(item => JavaHelpers.collFrom(item)).toArray, ErgoType.collType(ErgoType.byteType())))
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
   * returns watcher WID in register R4 of fraud box
   */
  def getWID: Array[Byte] = fraudBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Coll[Byte]]].toArray(0).toArray.clone()

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

class RWTRepoBox(repoBox: InputBox) extends ErgoBox(repoBox) {

  /**
   * returns array of watcher WIDs in register R4 of repo box
   */
  def getWIDs: Array[Array[Byte]] = repoBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Coll[Byte]]].toArray.map(item => item.toArray)

  /**
   * returns the number of RWTs for each WID in register R5 of repo box
   */
  def getRWTs: Array[Long] = repoBox.getRegisters.get(1).getValue.asInstanceOf[Coll[Long]].toArray.clone()

  /**
   * returns register R6
   */
  def getR6Values: Array[Long] = repoBox.getRegisters.get(2).getValue.asInstanceOf[Coll[Long]].toArray.clone()

  /**
   * returns price of RWT in RSN (first value of register R6)
   */
  def getRSNFactor: Long = getR6Values.head

  /**
   * creates new repo box using current repo box with new WIDs and RWTs passed by as arguments
   * @param txB transaction builder
   * @param WIDs array of watchers' WIDs in register R4
   * @param RWTs array of watchers' RWT count in register R5
   * @param watcherIndex the slashed WID index in array of watcher WIDs
   */
  def createRepoBox(txB: UnsignedTransactionBuilder, WIDs: Array[Array[Byte]], RWTs: Array[Long], watcherIndex: Int): OutBox = {
    val R4 = WIDs.map(item => JavaHelpers.SigmaDsl.Colls.fromArray(item))
    val R5 = JavaHelpers.SigmaDsl.Colls.fromArray(RWTs)
    val R6 = JavaHelpers.SigmaDsl.Colls.fromArray(getR6Values)

    txB.outBoxBuilder()
      .value(Configs.minBoxValue)
      .tokens(
        new ErgoToken(Configs.tokens.RepoNFT, 1),
        new ErgoToken(Configs.tokens.RWT, RWTAmount + 1),
        new ErgoToken(Configs.tokens.RSN, RSNAmount - this.getRSNFactor)
      )
      .contract(Contracts.RWTRepo)
      .registers(
        ErgoValue.of(R4, ErgoType.collType(ErgoType.byteType())),
        ErgoValue.of(R5, ErgoType.longType()),
        ErgoValue.of(R6, ErgoType.longType()),
        ErgoValue.of(watcherIndex)
      ).build()
  }

  /**
   * returns the amount RWT in repo box
   */
  private def RWTAmount: Long = repoBox.getTokens.asScala.find(token => token.getId.toString == Configs.tokens.RWT)
    .getOrElse(throw UnexpectedException(s"Token RWT ${Configs.tokens.RWT} not found in repoBox with Id ${repoBox.getId.toString}"))
    .getValue

  /**
   * returns the amount RSN in repo box
   */
  private def RSNAmount: Long = repoBox.getTokens.asScala.find(token => token.getId.toString == Configs.tokens.RSN)
    .getOrElse(throw UnexpectedException(s"Token RSN ${Configs.tokens.RSN} not found in repoBox with Id ${repoBox.getId.toString}"))
    .getValue

}
