package network

import helpers.{Configs, RosenLogging, Utils}
import helpers.RosenExceptions._
import org.ergoplatform.appkit.BoxOperations.ExplorerApiUnspentLoader
import org.ergoplatform.appkit.impl.ExplorerAndPoolUnspentBoxesLoader
import org.ergoplatform.appkit.{Address, BoxOperations, CoveringBoxes, ErgoClient, ErgoToken, InputBox, RestApiErgoClient}
import rosen.bridge.Contracts

import scala.collection.JavaConverters._

class Client extends RosenLogging {

  private val client: ErgoClient = try {
    val client = RestApiErgoClient.create(Configs.node.url, Configs.node.networkType, "", Configs.explorer)
    // test client connection by getting blockchain height, exit app on failure
    client.execute(ctx => {
      ctx.getHeight
    })
    client
  } catch {
    case e: Throwable =>
      log.error(s"Failed to set and interact with client! ${e.getMessage}.")
      sys.exit(1)
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
   * @return List of trigger event boxes (does not consider mempool)
   */
  def getEventBoxes: Seq[InputBox] = getUnspentBoxesFor(Utils.generateAddress(Contracts.EventTrigger), (1e9 * 1e8).toLong).getBoxes.asScala

  /**
   * @return List of fraud boxes (does not consider mempool)
   */
  def getFraudBoxes: Seq[InputBox] = getUnspentBoxesFor(Utils.generateAddress(Contracts.Fraud), (1e9 * 1e8).toLong).getBoxes.asScala

  /**
   * @return Last cleaner box (consider mempool)
   */
  def getCleanerBox: InputBox = {
    println(Configs.cleaner.address)
    getUnspentBoxesFor(
      new Address(Utils.getAddress(Configs.cleaner.address)),
      (1e9 * 1e8).toLong,
      Seq(new ErgoToken(Configs.tokens.CleanupNFT, 1)),
      considerMempool = true
    ).getBoxes.asScala.last
  }

  /**
   * @return Last repo box (consider mempool)
   */
  def getRepoBox: InputBox = getUnspentBoxesFor(
    Utils.generateAddress(Contracts.RWTRepo),
    (1e9 * 1e8).toLong,
    Seq(new ErgoToken(Configs.tokens.RepoNFT, 1)),
    considerMempool = true
  ).getBoxes.asScala.last

  /**
   * @return List of input boxes owned by cleaner and doesn't contain CleanupNFT (does not consider mempool)
   */
  def getCleanerFeeBoxes: Seq[InputBox] = getUnspentBoxesFor(Address.create(Configs.cleaner.address), (1e9 * 1e8).toLong).getBoxes.asScala
    .filterNot(box => box.getTokens.asScala.map(_.getId.toString).contains(Configs.tokens.CleanupNFT))

}
