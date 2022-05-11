package rosen.cleanup

import helpers.RosenExceptions.{FailedTxException, NotEnoughErgException, UnexpectedException}
import helpers.{Configs, RosenLogging}
import models.{RWTRepoBox, CleanerBox, FraudBox, TriggerEventBox}
import network.Client
import org.ergoplatform.appkit.{BlockchainContext, InputBox}

class Procedures(client: Client, transactions: Transactions) extends RosenLogging {

  private var cleanerBox: CleanerBox = new CleanerBox(client.getCleanerBox)
  private var repoBox: RWTRepoBox = new RWTRepoBox(client.getRepoBox)

  /**
   * returns cleaner box
   */
  def getCleanerBox: CleanerBox = cleanerBox

  /**
   * returns cleaner box
   */
  def getRepoBox: RWTRepoBox = repoBox

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
        case e: NotEnoughErgException =>
          log.error(s"Aborting process. Reason: ${e.getMessage}")

        case e: Throwable =>
          log.error(s"An error occurred while moving eventBox ${box.getId} to fraud.\n${e.getMessage}")

          try {
            client.getUnspentBoxById(cleanerBox.getId)
            log.warn(s"No problem found with the cleaner box. Aborting process.")
          }
          catch {
            case _: UnexpectedException =>
              log.warn(s"It seems cleanerBox ${cleanerBox.getId} got forked. Re-initiating the box and trying again...")
              cleanerBox = new CleanerBox(client.getCleanerBox)

              try {
                moveToFraud(ctx, box)
              }
              catch {
                case e: Throwable =>
                  log.error(s"Failed second try. Aborting process. Reason: ${e.getMessage}")
              }

            case e: Throwable =>
              log.error(s"Aborting process. Reason: ${e.getMessage}")
          }
      }
    })
  }

  /**
   * get the other boxes of cleaner if needed to pay transaction fee (throw exception if there is not enough erg in address)
   * @return if there is enough erg in cleaner box returns empty list, otherwise get feeBoxes in network
   */
  private def getCleanerFeeBoxes: Seq[InputBox] = {
    // check if there is enough erg in cleaner box
    if (cleanerBox.hasEnoughErg()) Seq.empty[InputBox]
    else {
      // get all other boxes owned by cleaner
      val feeBoxes: Seq[InputBox] = client.getCleanerFeeBoxes

      // check if there is enough erg with additional boxes
      val feeBoxesErgs: Long = feeBoxes.map(_.getValue).sum
      if (!cleanerBox.hasEnoughErg(feeBoxesErgs)) throw NotEnoughErgException(cleanerBox.getErgs + feeBoxesErgs, Configs.minBoxValue + Configs.fee)
      else feeBoxes
    }
  }

  /**
   * generates fraud box for every RWT in the trigger event box
   *
   * @param ctx blockchain context
   * @param eventBox the trigger event box
   */
  private def moveToFraud(ctx: BlockchainContext, eventBox: TriggerEventBox): Unit = {
    // get feeBoxes if needed
    val feeBoxes: Seq[InputBox] = getCleanerFeeBoxes

    // generate MoveToFraud tx
    val tx = transactions.generateFrauds(ctx, eventBox, cleanerBox, feeBoxes)

    // send tx
    try {
      ctx.sendTransaction(tx)
      log.info(s"MoveToFraud transaction sent. TxId: ${tx.getId}")

      // update cleaner box
      cleanerBox = new CleanerBox(tx.getOutputsToSpend.get(tx.getOutputsToSpend.size() - 2))
    }
    catch {
      case e: Throwable =>
        log.debug(s"failed to send MoveToFraud transaction. Error: $e")
        throw FailedTxException(s"txId: ${tx.getId}")
    }
  }

  /**
   * processes all frauds boxes that contains cleaner fraudToken, redeems and slashes their tokens
   *
   * @param ctx blockchain context
   */
  def processFrauds(ctx: BlockchainContext): Unit = {
    val fraudBoxes = client.getFraudBoxes.map(new FraudBox(_))

    fraudBoxes.foreach(box => {
      try {
        slashFraudBox(ctx, box)
      }
      catch {
        case e: NotEnoughErgException =>
          log.error(s"Aborting process. Reason: ${e.getMessage}")

        case e: Throwable =>
          log.error(s"An error occurred while merging fraud box ${box.getId} to repo ${repoBox.getId}.\n${e.getMessage}")

          try {
            client.getUnspentBoxById(cleanerBox.getId)
            log.warn(s"No problem found with the cleaner box.")
            client.getUnspentBoxById(repoBox.getId)
            log.warn(s"No problem found with the repo box either. Aborting process...")
          }
          catch {
            case _: UnexpectedException =>
              log.warn(s"It seems cleanerBox or repoBox got forked. Re-initiating the boxes and trying again...")
              cleanerBox = new CleanerBox(client.getCleanerBox)
              repoBox = new RWTRepoBox(client.getRepoBox)

              try {
                slashFraudBox(ctx, box)
              }
              catch {
                case e: Throwable =>
                  log.error(s"Failed second try. Aborting process. Reason: ${e.getMessage}")
              }

            case e: Throwable =>
              log.error(s"Aborting process. Reason: ${e.getMessage}")
          }
      }
    })
  }

  /**
   * redeems and slashes fraud box tokens
   *
   * @param ctx blockchain context
   * @param fraudBox the fraud box
   */
  private def slashFraudBox(ctx: BlockchainContext, fraudBox: FraudBox): Unit = {
    // get feeBoxes if needed
    val feeBoxes: Seq[InputBox] = getCleanerFeeBoxes

    // generate MoveToFraud tx
    val tx = transactions.slashFraud(ctx, fraudBox, repoBox, cleanerBox, feeBoxes)

    // send tx
    try {
      ctx.sendTransaction(tx)
      log.info(s"SlashFraudBox transaction sent. TxId: ${tx.getId}")

      // update repo box and cleaner box
      val txOutputs = tx.getOutputsToSpend
      cleanerBox = new CleanerBox(txOutputs.get(2))
      repoBox = new RWTRepoBox(txOutputs.get(0))
    }
    catch {
      case e: Throwable =>
        log.debug(s"failed to send SlashFraudBox transaction. Error: $e")
        throw FailedTxException(s"txId: ${tx.getId}")
    }
  }

}
