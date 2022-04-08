package rosen.cleanup

import helpers.RosenExceptions.{failedTxException, notEnoughErgException, unexpectedException}
import helpers.{Configs, RosenLogging, Utils}
import models.{CleanerBox, TriggerEventBox}
import network.{Client, Explorer}
import org.ergoplatform.appkit.{Address, BlockchainContext, ErgoTreeTemplate, InputBox}
import rosen.bridge.Contracts
import scorex.crypto.hash.Sha256
import scorex.util.encode.Base16
import sigmastate.Values.ErgoTree

class Procedures(client: Client, explorer: Explorer, transactions: Transactions) extends RosenLogging {
  var cleanerBoxId: String = ""

  def initCleanerBoxId(): Unit = {
    val ergoTree: ErgoTree = Address.create(Configs.cleaner.address).getErgoAddress.script
    val ergoTreeTemplateHash = Base16.encode(Sha256(ErgoTreeTemplate.fromErgoTree(ergoTree).getBytes))
    cleanerBoxId = explorer.getUnspentTokenBoxIdsForAddress(ergoTreeTemplateHash, Configs.tokens.CleanupNFT).headOption
      .getOrElse(throw unexpectedException(s"no box found containing CleanupNFT ${Configs.tokens.CleanupNFT} for address ${Configs.cleaner.address}"))
    // TODO: track mempool
  }

  /**
   * processes trigger event boxes, moves them to fraud if it's old enough
   *
   * @param ctx blockchain context
   */
  def processEvents(ctx: BlockchainContext): Unit = {
    val height = client.getHeight
    val triggerEventBoxes = client.getUnspentBoxForAddress(Utils.generateAddress(Contracts.WatcherTriggerEvent))
      .filter(box => height - box.getCreationHeight.toLong > Configs.cleanupConfirm)
    // TODO: filter the boxes that are already in mempool
      .map(new TriggerEventBox(_))

    triggerEventBoxes.foreach(box => {
      try {
        moveToFraud(ctx, box)
      }
      catch {
        case e: notEnoughErgException =>
          log.error(s"Aborting process. Reason: ${e.getMessage}")

        case e: Throwable =>
          log.error(s"An error occurred while moving eventBox ${box.getId} to fraud.\n${e.getMessage}")
      }
    })
  }

  /**
   * generates fraud box for every EWR in the trigger event box
   *
   * @param ctx blockchain context
   * @param eventBox the trigger event box
   */
  private def moveToFraud(ctx: BlockchainContext, eventBox: TriggerEventBox): Unit = {
    // get cleanerNFT box TODO: track mempool
    val cleanerBox = new CleanerBox(ctx.getBoxesById(cleanerBoxId).headOption
      .getOrElse(throw unexpectedException(s"no box found with id $cleanerBoxId")))

    // check if there is enough fraudTokens
    val watchersLen = eventBox.getWatchersLen
    if (cleanerBox.hasEnoughFraudToken(watchersLen)) {
      log.warn(s"Not enough fraudToken. Contains ${cleanerBox.getFraudTokens}, required $watchersLen. Aborting moveToFraud for eventBox ${eventBox.getId}")
      return
    }

    // check if there is enough erg in cleaner box
    val feeBoxes: Seq[InputBox] = if (cleanerBox.hasEnoughErg()) Seq.empty[InputBox]
    else {
      // get all other boxes owned by cleaner
      val feeBoxes: Seq[InputBox] = client.getCleanerFeeBoxes

      // check if there is enough erg with additional boxes
      val feeBoxesErgs: Long = feeBoxes.map(_.getValue).sum
      if (!cleanerBox.hasEnoughErg(feeBoxesErgs)) throw notEnoughErgException(cleanerBox.getErgs + feeBoxesErgs, Configs.minBoxValue + Configs.fee)
      else feeBoxes
    }

    // generate MoveToFraud tx
    val tx = transactions.generateFrauds(ctx, eventBox, cleanerBox, feeBoxes)

    // send tx
    try {
      ctx.sendTransaction(tx)
    }
    catch {
      case e: Throwable =>
        log.debug(s"failed to send MoveToFraud transaction. Error: $e")
        throw failedTxException(s"txId: ${tx.getId}")
    }
  }

  /**
   * process all frauds boxes that contains cleaner fraudToken, merges them to bank
   *
   * @param ctx blockchain context
   */
  def processFrauds(ctx: BlockchainContext): Unit = {
    // get every frauds that contains cleaner fraudToken

    // get bank box (track mempool)

    // generate mergeFraud tx
    // send tx
  }
}
