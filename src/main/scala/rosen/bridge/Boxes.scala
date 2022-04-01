package rosen.bridge

import helpers.Configs
import network.Explorer
import org.ergoplatform.appkit.{ErgoToken, ErgoType, ErgoValue, InputBox, JavaHelpers, OutBox, UnsignedTransactionBuilder}
import special.collection.Coll

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Random

class Boxes(explorer: Explorer) {

  def getRandomHexString(length: Int = 64): String = {
    val r = new Random()
    val sb = new StringBuffer
    while ( {
      sb.length < length
    }) sb.append(Integer.toHexString(r.nextInt))
    sb.toString.substring(0, length)
  }

  def createFraudBox(txB: UnsignedTransactionBuilder, EWRId: String, UTP: Array[Byte]): OutBox = {
    txB.outBoxBuilder()
      .value(Configs.minBoxValue)
      .contract(Contracts.WatcherFraudLock)
      .tokens(new ErgoToken(EWRId, 1))
      .registers(ErgoValue.of(Seq(UTP).map(item => JavaHelpers.SigmaDsl.Colls.fromArray(item)).toArray, ErgoType.collType(ErgoType.byteType())))
      .build()
  }

  def createBankBox(txB: UnsignedTransactionBuilder, EWRTokenId: String, EWRCount: Long,
                    RSNCount: Long, users: Seq[Array[Byte]], userEWR: Seq[Long], R7: Int): OutBox = {
    val R4 = users.map(item => JavaHelpers.SigmaDsl.Colls.fromArray(item)).toArray
    val bankBuilder = txB.outBoxBuilder()
      .value(Configs.minBoxValue)
      .tokens(
        new ErgoToken(Configs.tokens.BankNft, 1),
        new ErgoToken(EWRTokenId, EWRCount)
      )
      .contract(Contracts.WatcherBank)
      .registers(
        ErgoValue.of(R4, ErgoType.collType(ErgoType.byteType())),
        ErgoValue.of(JavaHelpers.SigmaDsl.Colls.fromArray(userEWR.toArray), ErgoType.longType()),
        ErgoValue.of(JavaHelpers.SigmaDsl.Colls.fromArray(Array(100L, 51L)), ErgoType.longType()),
        ErgoValue.of(R7)
      )
    if(RSNCount > 0){
      bankBuilder.tokens(new ErgoToken(Configs.tokens.RSN, RSNCount))
    }
    bankBuilder.build()
  }

  def createLockBox(txB: UnsignedTransactionBuilder, EWRId: String, EWRCount: Long, UTP: Array[Byte], tokens: ErgoToken*): OutBox = {
    val tokensSeq = Seq(new ErgoToken(EWRId, EWRCount)) ++ tokens.toSeq
    txB.outBoxBuilder()
      .value(Configs.minBoxValue)
      .contract(Contracts.WatcherLock)
      .tokens(tokensSeq: _*)
      .registers(ErgoValue.of(Seq(UTP).map(item => JavaHelpers.SigmaDsl.Colls.fromArray(item)).toArray, ErgoType.collType(ErgoType.byteType())))
      .build()
  }

  def getTokenCount(TokenId: String, box: InputBox): Long = {
    val EWRToken = box.getTokens.asScala.filter(token => token.getId.toString == TokenId).toArray
    if(EWRToken.length == 0) 0 else EWRToken(0).getValue
  }

  def getEventBoxUTPs(eventBox: InputBox): Array[Array[Byte]] = eventBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Coll[Byte]]].toArray.map(item => item.toArray)

  def calcTotalErgAndTokens(boxes: Seq[InputBox]): mutable.Map[String, Long] ={
    val tokens: mutable.Map[String, Long] = mutable.Map()
    val totalErg = boxes.map(item => item.getValue.toLong).sum
    boxes.foreach(box => {
      box.getTokens.forEach(token => {
        val tokenId = token.getId.toString
        tokens.update(tokenId, tokens.getOrElse(tokenId, 0L) + token.getValue)
      })
    })
    tokens.update("", totalErg)
    tokens
  }

}
