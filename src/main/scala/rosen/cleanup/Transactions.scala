package rosen.cleanup

import helpers.Configs
import helpers.RosenExceptions.proveException
import org.ergoplatform.appkit.{BlockchainContext, InputBox, SignedTransaction}
import rosen.bridge.{Boxes, Contracts}

import scala.collection.JavaConverters._

class Transactions(boxes: Boxes) {

  /**
   * generates a transaction that spends triggerEvent box, cleanerBox and feeBoxes to create newFraud boxes
   *
   * @param ctx        blockchain context
   * @param eventBox   trigger event box
   * @param cleanerBox the box contains cleanup token
   * @param feeBoxes   other boxes of cleaner - empty list if cleaner box ergs is enough
   */
  def generateFrauds(ctx: BlockchainContext, eventBox: InputBox, cleanerBox: InputBox, feeBoxes: Seq[InputBox]): SignedTransaction = {
    val prover = ctx.newProverBuilder().withDLogSecret(Configs.cleaner.secret).build()
    val txB = ctx.newTxBuilder()

    val inputBoxes = (Seq(eventBox, cleanerBox) ++ feeBoxes).asJava

    val watcherUTPs = boxes.getEventBoxUTPs(eventBox)
    val newFrauds = watcherUTPs.map(item => {
      boxes.createFraudBox(txB, Configs.EWRId, item)
    })

    val unsignedTx = txB.boxesToSpend(inputBoxes)
      .fee(Configs.fee)
      .sendChangeTo(prover.getAddress.getErgoAddress)
      .outputs(newFrauds: _*)
      .build()

    try {
      val tx = prover.sign(unsignedTx)
      println(s"MoveToFraud tx generated. txId: ${tx.getId}")
      tx
    } catch {
      case e: Throwable =>
        println(s"MoveToFraud tx proving failed. error message: ${e.getMessage}")
        throw proveException()
    }
  }

  /*def mergeFraud(): SignedTransaction = {

  }*/

}
