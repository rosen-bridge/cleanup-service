package models

import helpers.Configs
import helpers.RosenExceptions.unexpectedException
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
        .tokens(
          new ErgoToken(Configs.tokens.EWR, 1),
          new ErgoToken(Configs.cleaner.fraudToken, 1)
        )
        .registers(ErgoValue.of(Seq(UTP).map(item => JavaHelpers.SigmaDsl.Colls.fromArray(item)).toArray, ErgoType.collType(ErgoType.byteType())))
        .build()
    }).toSeq
  }

}

class CleanerBox(cleanerBox: InputBox) extends ErgoBox(cleanerBox) {

  /**
   * returns number of fraudTokens in box
   */
  def getFraudTokens: Long = getTokens.find(token => token.getId.toString == Configs.cleaner.fraudToken)
    .getOrElse(throw unexpectedException(s"cleaner box is not containing fraudToken ${Configs.cleaner.fraudToken}"))
    .getValue

  /**
   * returns true if the box has enough erg to create new cleanerBox and pay tx fee
   * @param feeBoxesErgs the amount of erg in the additional boxes of cleaner
   */
  def hasEnoughErg(feeBoxesErgs: Long = 0): Boolean = cleanerBox.getValue + feeBoxesErgs >= Configs.minBoxValue + Configs.fee

  /**
   * returns true if the box has NOT enough fraud token to create new fraud boxes for the event (and remain one token)
   * @param watchersLen number of watchers in the event
   */
  def notEnoughFraudToken(watchersLen: Long): Boolean = getFraudTokens <= watchersLen

  /**
   * creates new cleaner box using current cleaner box and feeBoxes and size of watchers in fraud trigger event box
   * @param txB transaction builder
   * @param watchersLen size of watchers in trigger event box
   * @param feeBoxes additional boxes of cleaner
   * @return
   */
  def createCleanerBox(txB: UnsignedTransactionBuilder, watchersLen: Int, feeBoxes: Seq[InputBox]): OutBox = {
    var tokensSeq: Seq[ErgoToken] = cleanerBox.getTokens.asScala.map(token => {
      if (token.getId.toString == Configs.cleaner.fraudToken) new ErgoToken(token.getId, token.getValue - watchersLen)
      else token
    })
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

  /**
   * adds a token to sequence of token (update sequence in case the token already exists)
   * @param tokenSeq sequence of tokens
   * @param boxToken token to be added
   * @return
   */
  private def addBoxToken(tokenSeq: Seq[ErgoToken], boxToken: ErgoToken): Seq[ErgoToken] = {
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

// TODO: Implement this class
class FraudBox(fraudBox: InputBox) extends ErgoBox(fraudBox) {

  /**
   * returns watcher UTP in register R4 of fraud box
   */
  def getUTP: Array[Byte] = fraudBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Coll[Byte]]].toArray(0).toArray.clone()

}
