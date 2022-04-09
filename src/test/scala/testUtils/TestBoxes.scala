package testUtils

import helpers.{Configs, Utils}
import models.{CleanerBox, TriggerEventBox}
import network.Client
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{ErgoContract, ErgoToken, ErgoType, ErgoValue, InputBox, JavaHelpers, OutBox, SignedTransaction}
import org.mockito.Mockito.{mock, when}
import rosen.bridge.Contracts
import scorex.util.encode.Base16

import scala.collection.JavaConverters._

object TestBoxes {

  private val client = new Client
  client.setClient()

  /**
   * generates random ID (for tokenId or TxId)
   */
  def generateRandomId: String = Base16.encode(Array.fill(32)((scala.util.Random.nextInt(256) - 128).toByte))

  /**
   * generates multiple IDs
   */
  def generateRandomIds(size: Int): Seq[String] = {
    var ids: Seq[String] = Seq.empty[String]
    for (_ <- 0 until size) {
      ids = ids :+ generateRandomId
    }
    ids
  }

  /**
   * generates multiple InputBoxes
   */
  def generateMockInputBoxes(size: Int): Seq[InputBox] = {
    var boxes: Seq[InputBox] = Seq.empty[InputBox]
    for (_ <- 0 until size) {
      boxes = boxes :+ mock(classOf[InputBox])
    }
    boxes
  }

  /**
   * creates an OutBox using parameters
   * @param value erg value
   * @param tokens box tokens
   * @param registers box registers
   * @param contract box contract
   * @param creationHeight box creationHeight
   */
  def createOutBox(value: Long, tokens: Seq[ErgoToken], registers: Seq[ErgoValue[_]], contract: ErgoContract, creationHeight: Int): OutBox = {
    client.getClient.execute(ctx => {
      var box = ctx.newTxBuilder().outBoxBuilder()
        .value(value)
        .contract(contract)
        .creationHeight(creationHeight)
      if (tokens.nonEmpty) box = box.tokens(tokens: _*)
      if (registers.nonEmpty) box = box.registers(registers: _*)
      box.build()
    })
  }

  /**
   * mocks a trigger event box (without R5 and R6)
   * @param watchersSize number of watchers (UTPs) in event box
   * @param creationHeight box creation height
   */
  def mockTriggerEventBox(watchersSize: Int, creationHeight: Int): TriggerEventBox = {
    val UTPs = generateRandomIds(watchersSize).map(Base16.decode(_).get)
    val R4 = UTPs.map(item => JavaHelpers.SigmaDsl.Colls.fromArray(item)).toArray
    new TriggerEventBox(createOutBox(
      Configs.minBoxValue * watchersSize,
      Seq(new ErgoToken(Configs.tokens.EWR, watchersSize)),
      Seq(ErgoValue.of(R4, ErgoType.collType(ErgoType.byteType()))),
      Contracts.WatcherTriggerEvent,
      creationHeight
    ).convertToInputWith(generateRandomId, 0))
  }

  /**
   * mocks a cleaner box
   * @param creationHeight box creation height
   */
  def mockCleanerBox(value: Long, creationHeight: Int): CleanerBox = {
    new CleanerBox(createOutBox(
      value,
      Seq(new ErgoToken(Configs.tokens.CleanupNFT, 1), new ErgoToken(Configs.cleaner.fraudToken, 1000000)),
      Seq.empty[ErgoValue[_]],
      new ErgoTreeContract(Utils.getAddressFromString(Configs.cleaner.address).script, Configs.node.networkType),
      creationHeight
    ).convertToInputWith(generateRandomId, 0))
  }

  /**
   * mocks two feeBoxes for cleaner address
   * @param creationHeight each box creation height
   */
  def mockFeeBoxes(creationHeight: Int): Seq[InputBox] = {
    Seq(
      createOutBox(
        Configs.minBoxValue * 10,
        Seq(new ErgoToken(generateRandomId, 100)),
        Seq.empty[ErgoValue[_]],
        new ErgoTreeContract(Utils.getAddressFromString(Configs.cleaner.address).script, Configs.node.networkType),
        creationHeight
      ).convertToInputWith(generateRandomId, 0),
      createOutBox(
        Configs.minBoxValue * 5,
        Seq.empty[ErgoToken],
        Seq.empty[ErgoValue[_]],
        new ErgoTreeContract(Utils.getAddressFromString(Configs.cleaner.address).script, Configs.node.networkType),
        creationHeight
      ).convertToInputWith(generateRandomId, 0)
    )
  }

  /**
   * mocks one feeBox for cleaner address
   * @param creationHeight each box creation height
   */
  def mockMinimumFeeBox(creationHeight: Int): InputBox = {
    createOutBox(
      Configs.minBoxValue,
      Seq.empty[ErgoToken],
      Seq.empty[ErgoValue[_]],
      new ErgoTreeContract(Utils.getAddressFromString(Configs.cleaner.address).script, Configs.node.networkType),
      creationHeight
    ).convertToInputWith(generateRandomId, 0)
  }

  /**
   * mocks a moveToFraud transaction (only cleaner box is valid)
   * @param cleanerBox
   * @param watchersLen
   * @return
   */
  def mockMoveToFraudTransaction(cleanerBox: CleanerBox, watchersLen: Int): SignedTransaction = {
    // generate random id for transaction
    val txId = generateRandomId

    // creates new cleaner box
    val newCleanerBox = client.getClient.execute(ctx => {
      val txB = ctx.newTxBuilder()
      cleanerBox.createCleanerBox(txB, watchersLen, Seq.empty[InputBox])
    }).convertToInputWith(txId, watchersLen.toShort)

    // mock transaction and its methods
    val mockedSignedTransaction = mock(classOf[SignedTransaction])
    val mockedListInputBoxes = generateMockInputBoxes(watchersLen) ++ Seq(newCleanerBox) ++ generateMockInputBoxes(1)
    when(mockedSignedTransaction.getOutputsToSpend).thenReturn(mockedListInputBoxes.asJava)
    when(mockedSignedTransaction.getId).thenReturn(txId)
    mockedSignedTransaction
  }

}
