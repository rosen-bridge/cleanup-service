package rosen.cleanup

import helpers.RosenExceptions.{failedTxException, notEnoughErgException, unexpectedException}
import helpers.{Configs, RosenLogging}
import models.{CleanerBox, TriggerEventBox}
import network.Client
import org.ergoplatform.appkit.{BlockchainContext, InputBox}
class Procedures(client: Client, transactions: Transactions) extends RosenLogging {
  var cleanerBox: CleanerBox = new CleanerBox(client.getCleanerBox)

  /**
   * processes trigger event boxes, moves them to fraud if it's old enough
   *
   * @param ctx blockchain context
   */
  def processEvents(ctx: BlockchainContext): Unit = {
    val height = client.getHeight
    val triggerEventBoxes = client.getEventBoxes
      .filter(box => height - box.getCreationHeight.toLong > Configs.cleanupConfirm)
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

          try {
            client.getUnspentBoxesFor(cleanerBox.getId)
            log.warn(s"No problem found with the cleaner box. Aborting process.")
          }
          catch {
            case _: unexpectedException =>
              log.warn(s"It seems cleanerBox ${cleanerBox.getId} got forked. Re-initiating the box and trying again...")
              cleanerBox = new CleanerBox(client.getCleanerBox)

              try {
                moveToFraud(ctx, box)
              }
              catch {
                case e: notEnoughErgException =>
                  log.error(s"Failed second try. Aborting process. Reason: ${e.getMessage}")
              }

            case e: Throwable =>
              log.error(s"Aborting process. Reason: ${e.getMessage}")
          }
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
