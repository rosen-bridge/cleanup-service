package testUtils

import helpers.{Configs, Utils}
import models.{BankBox, CleanerBox, FraudBox, TriggerEventBox}
import network.Client
import org.ergoplatform.appkit.{ErgoContract, ErgoToken, ErgoType, ErgoValue, InputBox, JavaHelpers, OutBox, SignedTransaction}
import org.mockito.Mockito.{mock, when}
import rosen.bridge.Contracts
import scorex.util.encode.Base16

import scala.collection.JavaConverters._

object TestBoxes {

  val client = new Client

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
      Seq(new ErgoToken(Configs.tokens.CleanupNFT, 1)),
      Seq.empty[ErgoValue[_]],
      Utils.getAddressContract(Configs.cleaner.address),
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
        Utils.getAddressContract(Configs.cleaner.address),
        creationHeight
      ).convertToInputWith(generateRandomId, 0),
      createOutBox(
        Configs.minBoxValue * 5,
        Seq.empty[ErgoToken],
        Seq.empty[ErgoValue[_]],
        Utils.getAddressContract(Configs.cleaner.address),
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
      Utils.getAddressContract(Configs.cleaner.address),
      creationHeight
    ).convertToInputWith(generateRandomId, 0)
  }

  /**
   * mocks a bank box
   * @param watchersSize number of watchers (UTPs) in bank box
   * @param creationHeight box creation height
   */
  def mockBankBox(watchersSize: Int, creationHeight: Int): BankBox = {
    val UTPs = generateRandomIds(watchersSize).map(Base16.decode(_).get)
    val R4 = UTPs.map(item => JavaHelpers.SigmaDsl.Colls.fromArray(item)).toArray
    val EWRs = (1 to watchersSize).map(_.toLong).toArray
    val R5 = JavaHelpers.SigmaDsl.Colls.fromArray(EWRs)
    new BankBox(createOutBox(
      Configs.minBoxValue * watchersSize,
      Seq(
        new ErgoToken(Configs.tokens.BankNft, 1),
        new ErgoToken(Configs.tokens.EWR, 100),
        new ErgoToken(Configs.tokens.RSN, 10000)
      ),
      Seq(
        ErgoValue.of(R4, ErgoType.collType(ErgoType.byteType())),
        ErgoValue.of(R5, ErgoType.longType()),
        ErgoValue.of(JavaHelpers.SigmaDsl.Colls.fromArray(Array(100L, 51L, 0L, 9999L)), ErgoType.longType()),
        ErgoValue.of(watchersSize - 1)
      ),
      Contracts.WatcherBank,
      creationHeight
    ).convertToInputWith(generateRandomId, 0))
  }

  /**
   * mocks a fraud box
   * @param UTP watcher's UTP
   * @param creationHeight box creation height
   */
  def mockFraudBox(UTP: Array[Byte], creationHeight: Int): FraudBox = {
    new FraudBox(createOutBox(
      Configs.minBoxValue,
      Seq(new ErgoToken(Configs.tokens.EWR, 1)),
      Seq(ErgoValue.of(Seq(UTP).map(item => JavaHelpers.SigmaDsl.Colls.fromArray(item)).toArray, ErgoType.collType(ErgoType.byteType()))),
      Contracts.WatcherFraudLock,
      creationHeight
    ).convertToInputWith(generateRandomId, 0))
  }

  /**
   * mocks a moveToFraud transaction (only cleaner box is valid)
   * @param cleanerBox the cleaner box in input
   * @param watchersLen number of watchers in transaction event box
   */
  def mockMoveToFraudTransaction(cleanerBox: CleanerBox, watchersLen: Int): SignedTransaction = {
    // generate random id for transaction
    val txId = generateRandomId

    // creates new cleaner box
    val newCleanerBox = client.getClient.execute(ctx => {
      val txB = ctx.newTxBuilder()
      cleanerBox.createCleanerBox(txB, Seq.empty[InputBox])
    }).convertToInputWith(txId, watchersLen.toShort)

    // mock transaction and its methods
    val mockedSignedTransaction = mock(classOf[SignedTransaction])
    val mockedListInputBoxes = generateMockInputBoxes(watchersLen) ++ Seq(newCleanerBox) ++ generateMockInputBoxes(1)
    when(mockedSignedTransaction.getOutputsToSpend).thenReturn(mockedListInputBoxes.asJava)
    when(mockedSignedTransaction.getId).thenReturn(txId)
    mockedSignedTransaction
  }

  /**
   * mocks a slashFraud transaction (only cleaner box and bank box are valid, reducing last watcher EWR count)
   * @param cleanerBox the cleaner box in input
   */
  def mockSlashFraudTransaction(bankBox: BankBox, cleanerBox: CleanerBox): SignedTransaction = {
    // generate random id for transaction
    val txId = generateRandomId

    // creates new cleaner box
    val newCleanerBox = client.getClient.execute(ctx => {
      val txB = ctx.newTxBuilder()
      cleanerBox.createCleanerBox(txB, Seq.empty[InputBox])
    }).convertToInputWith(txId, 2)

    // creates new cleaner box
    val newBankBox = client.getClient.execute(ctx => {
      val txB = ctx.newTxBuilder()
      val newEWRs = bankBox.getEWRs.slice(0, bankBox.getEWRs.length - 1) :+ (bankBox.getEWRs.last - 1L)
      bankBox.createBankBox(txB, bankBox.getUTPs, newEWRs, bankBox.getEWRs.length - 1)
    }).convertToInputWith(txId, 0)

    // mock transaction and its methods
    val mockedSignedTransaction = mock(classOf[SignedTransaction])
    val mockedListInputBoxes = Seq(newBankBox) ++ generateMockInputBoxes(1) ++ Seq(newCleanerBox)
    when(mockedSignedTransaction.getOutputsToSpend).thenReturn(mockedListInputBoxes.asJava)
    when(mockedSignedTransaction.getId).thenReturn(txId)
    mockedSignedTransaction
  }

}
