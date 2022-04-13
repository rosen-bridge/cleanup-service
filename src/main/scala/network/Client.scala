package network


import helpers.{Configs, RosenLogging, Utils}
import helpers.RosenExceptions._
import org.ergoplatform.appkit.BoxOperations.ExplorerApiUnspentLoader
import org.ergoplatform.appkit.impl.ExplorerAndPoolUnspentBoxesLoader
import org.ergoplatform.appkit.{Address, BoxOperations, CoveringBoxes, ErgoClient, ErgoToken, InputBox, RestApiErgoClient}
import rosen.bridge.Contracts

import scala.collection.JavaConverters._

class Client extends RosenLogging {
  private var client: ErgoClient = _

  /**
   * Sets client for the entire app when the app starts
   *
   * @return current height of blockchain
   */
  def setClient(): Long = {
    try {
      client = RestApiErgoClient.create(Configs.node.url, Configs.node.networkType, "", Configs.explorer)
      client.execute(ctx => {
        ctx.getHeight
      })
    } catch {
      case e: Throwable =>
        log.error(s"Could not set client! ${e.getMessage}.")
        0L
    }
  }

  def getClient: ErgoClient = {
    client
  }

  /**
   * @return current height of the blockchain
   */
  def getHeight: Long = {
    try {
      client.execute(ctx => ctx.getHeight)
    } catch {
      case e: Throwable =>
        log.error(e.getMessage)
        throw ConnectionException()
    }
  }

  /**
   * @param boxId :String box Id
   * @return corresponding input box
   */
  def getUnspentBoxById(boxId: String): InputBox = client.execute(ctx => {
    try {
      ctx.getBoxesById(boxId).headOption.getOrElse(throw UnexpectedException(s"no unspent box found for id $boxId"))
    } catch {
      case e: UnexpectedException => throw e
      case e: Throwable =>
        log.error(e.getMessage)
        throw ConnectionException()
    }
  })

  /**
   * @param address :Address get a valid address
   * @return List of input address boxes covering the required amount
   */
  def getUnspentBoxesFor(address: Address, amount: Long, tokens: Seq[ErgoToken] = Seq.empty[ErgoToken], considerMempool: Boolean = false, changeBoxConsidered: Boolean = false): CoveringBoxes = client.execute(ctx => {
    try {
      val boxLoader = if (considerMempool) new ExplorerAndPoolUnspentBoxesLoader().withAllowChainedTx(true)
      else new ExplorerApiUnspentLoader()
      BoxOperations.getCoveringBoxesFor(amount, tokens.asJava, changeBoxConsidered,
        (page: Integer) => { boxLoader.loadBoxesPage(ctx, address, page)})
    } catch {
      case e: Throwable =>
        log.error(e.getMessage)
        throw ConnectionException()
    }
  })

  /**
   * @return List of triger event boxes (does not consider mempool)
   */
  def getEventBoxes: Seq[InputBox] = getUnspentBoxesFor(Utils.generateAddress(Contracts.WatcherTriggerEvent), (1e9 * 1e8).toLong).getBoxes.asScala

  /**
   * @return List of fraud lock boxes (does not consider mempool)
   */
  def getFraudBoxes: Seq[InputBox] = getUnspentBoxesFor(Utils.generateAddress(Contracts.WatcherFraudLock), (1e9 * 1e8).toLong).getBoxes.asScala

  /**
   * @return Last cleaner box (consider mempool)
   */
  def getCleanerBox: InputBox = getUnspentBoxesFor(
    Utils.generateAddress(Contracts.WatcherTriggerEvent),
    (1e9 * 1e8).toLong,
    Seq(new ErgoToken(Configs.tokens.CleanupNFT, 1)),
    considerMempool = true
  ).getBoxes.asScala.last

  /**
   * @return Last bank box (consider mempool)
   */
  def getBankBox: InputBox = getUnspentBoxesFor(
    Utils.generateAddress(Contracts.WatcherBank),
    (1e9 * 1e8).toLong,
    Seq(new ErgoToken(Configs.tokens.BankNft, 1)),
    considerMempool = true
  ).getBoxes.asScala.last

  /**
   * @return List of input boxes owned by cleaner and doesn't contain CleanupNFT (does not consider mempool)
   */
  def getCleanerFeeBoxes: Seq[InputBox] = getUnspentBoxesFor(Address.create(Configs.cleaner.address), (1e9 * 1e8).toLong).getBoxes.asScala
    .filterNot(box => box.getTokens.asScala.map(_.getId.toString).contains(Configs.tokens.CleanupNFT))

}
