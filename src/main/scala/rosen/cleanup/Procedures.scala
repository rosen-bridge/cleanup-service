package rosen.cleanup

import helpers.RosenExceptions.unexpectedException
import helpers.{Configs, Utils}
import network.{Client, Explorer}
import org.ergoplatform.appkit.{BlockchainContext, InputBox}
import rosen.bridge.{Boxes, Contracts}

class Procedures(client: Client, explorer: Explorer, boxes: Boxes) {

  /**
   * processes trigger event boxes, moves them to fraud if it's old enough
   *
   * @param ctx blockchain context
   */
  def processEvents(ctx: BlockchainContext): Unit = {
    val height = client.getHeight
    val triggerEventBoxes = client.getUnspentBoxForAddress(Utils.generateAddress(Contracts.WatcherTriggerEvent))
      .filter(box => height - box.getCreationHeight.toLong > Configs.fraudNum)

    triggerEventBoxes.foreach(box => moveToFraud(ctx, box))
  }

  /**
   * generates fraud box for every EWR in the trigger event box
   *
   * @param ctx blockchain context
   * @param eventBox the trigger event box
   */
  private def moveToFraud(ctx: BlockchainContext, eventBox: InputBox): Unit = {
    // get cleanerNFT box (track mempool)
    val cleanerBoxId = explorer.getUnspentTokenBoxIds(Configs.cleaner.token).headOption
      .getOrElse(throw unexpectedException(s"no box found containing cleaner token ${Configs.cleaner.token}"))
    val cleanerBox = ctx.getBoxesById(cleanerBoxId)

    val watchersSize = boxes.getEventBoxUTPs(eventBox).length

    // check if erg is enough for fee and minimum ergBox
    //    get all other boxes of cleaner
    //    throw feeException if still not enough erg

    // generate MoveToFraud tx
    // send tx
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
