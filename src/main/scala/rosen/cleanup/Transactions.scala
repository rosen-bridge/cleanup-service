package rosen.cleanup

import helpers.{Configs, RosenLogging}
import helpers.RosenExceptions.proveException
import org.ergoplatform.appkit.{BlockchainContext, InputBox, SignedTransaction}
import models._

import scala.collection.JavaConverters._

class Transactions extends RosenLogging {

  /**
   * generates a transaction that spends triggerEvent box, cleanerBox and feeBoxes to create newFraud boxes
   *
   * @param ctx        blockchain context
   * @param eventBox   trigger event box
   * @param cleanerBox the box contains cleanup token
   * @param feeBoxes   other boxes of cleaner - empty list if cleaner box ergs is enough
   */
  def generateFrauds(ctx: BlockchainContext, eventBox: TriggerEventBox, cleanerBox: CleanerBox, feeBoxes: Seq[InputBox]): SignedTransaction = {
    val prover = ctx.newProverBuilder().withDLogSecret(Configs.cleaner.secret).build()
    val txB = ctx.newTxBuilder()

    val watchersLen = eventBox.getWatchersLen

    // generate cleaner box
    val newCleanerBox = cleanerBox.createCleanerBox(txB, watchersLen, feeBoxes)

    // generate fraud boxes, form outputBoxes with new cleaner box
    val outputBoxes = eventBox.createFraudBoxes(txB) :+ newCleanerBox

    // generate tx
    val unsignedTx = txB.boxesToSpend((Seq(eventBox.getBox, cleanerBox.getBox) ++ feeBoxes).asJava)
      .fee(Configs.fee)
      .sendChangeTo(prover.getAddress.getErgoAddress)
      .outputs(outputBoxes: _*)
      .build()

    try {
      val tx = prover.sign(unsignedTx)
      log.info(s"MoveToFraud tx generated. txId: ${tx.getId}")
      tx
    } catch {
      case e: Throwable =>
        log.error(s"MoveToFraud tx proving failed. error message: ${e.getMessage}")
        throw proveException()
    }
  }

  /*def mergeFraud(): SignedTransaction = {

  }*/

}
