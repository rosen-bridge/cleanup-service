package network


import helpers.{Configs, RosenLogging}
import helpers.RosenExceptions._
import org.ergoplatform.appkit.{Address, BoxOperations, CoveringBoxes, ErgoClient, ErgoToken, InputBox, RestApiErgoClient}

import scala.collection.JavaConverters._

class Client extends RosenLogging {
  private var client: ErgoClient = _
  private val DEFAULT_LIMIT_FOR_API = 100

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
        throw connectionException()
    }
  }

  /**
   * @param address :Address get a valid address
   * @return List of input address boxes (maximum 100 boxes)
   */
  def getUnspentBox(address: Address, offset: Int = 0, limit: Int = DEFAULT_LIMIT_FOR_API): Seq[InputBox] = {
    client.execute(ctx =>
      try {
        ctx.getUnspentBoxesFor(address, offset, limit).asScala
      } catch {
        case e: Throwable =>
          log.error(e.getMessage)
          throw connectionException()
      }
    )
  }

  /**
   * @param address :Address get a valid address
   * @return List of input address boxes covering the required amount
   */
  def getCoveringBoxesFor(address: Address, amount: Long, changeBoxConsidered: Boolean = false): CoveringBoxes = {
    try {
      BoxOperations.getCoveringBoxesFor(amount, List[ErgoToken]().asJava, changeBoxConsidered,
        (page: Integer) => { getUnspentBox(address, page * DEFAULT_LIMIT_FOR_API, DEFAULT_LIMIT_FOR_API).asJava })
    } catch {
      case e: Throwable =>
        log.error(e.getMessage)
        throw connectionException()
    }
  }

  /**
   * @param address :Address get a valid address
   * @return List of input address boxes (maximum 100 boxes)
   */
  def getUnspentBoxForAddress(address: Address): Seq[InputBox] = getCoveringBoxesFor(address, (1e9 * 1e8).toLong).getBoxes.asScala

  /**
   * @return List of input boxes owned by cleaner and doesn't contain CleanupNFT
   */
  def getCleanerFeeBoxes: Seq[InputBox] = getCoveringBoxesFor(Address.create(Configs.cleaner.address), (1e9 * 1e8).toLong).getBoxes.asScala
    .filterNot(box => box.getTokens.asScala.map(_.getId.toString).contains(Configs.tokens.CleanupNFT))

}
