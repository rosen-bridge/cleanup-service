package rosen.cleanup

import helpers.{Configs, RosenLogging, Utils}
import helpers.RosenExceptions.{proveException, unexpectedException}
import org.ergoplatform.appkit.{BlockchainContext, InputBox, OutBox, SignedTransaction, UnsignedTransaction}
import models._
import scorex.util.encode.Base16

import scala.collection.JavaConverters._

class Transactions extends RosenLogging {

  /**
   * signs an unsigned transaction using cleaner secret
   *
   * @param ctx blockchain context
   * @param unsignedTx the unsigned transaction to be signed
   * @param txName transaction name to log (either 'MoveToFraud' or 'MergeFraudToBank')
   */
  def signTransaction(ctx: BlockchainContext, unsignedTx: UnsignedTransaction, txName: String): SignedTransaction = {
    val prover = ctx.newProverBuilder().withDLogSecret(Configs.cleaner.secret).build()
    try {
      val tx = prover.sign(unsignedTx)
      log.info(s"$txName tx generated. txId: ${tx.getId}")
      tx
    } catch {
      case e: Throwable =>
        log.error(s"$txName tx proving failed. error message: ${e.getMessage}")
        throw proveException()
    }
  }

  /**
   * generates a transaction that spends triggerEvent box, cleanerBox and feeBoxes to create newFraud boxes
   *
   * @param ctx        blockchain context
   * @param eventBox   trigger event box
   * @param cleanerBox the box contains cleanup token
   * @param feeBoxes   other boxes of cleaner - empty list if cleaner box ergs is enough
   */
  def generateFrauds(ctx: BlockchainContext, eventBox: TriggerEventBox, cleanerBox: CleanerBox, feeBoxes: Seq[InputBox]): SignedTransaction = {
    val txB = ctx.newTxBuilder()

    // generate cleaner box
    val newCleanerBox = cleanerBox.createCleanerBox(txB, feeBoxes)

    // generate fraud boxes, form outputBoxes with new cleaner box
    val outputBoxes = eventBox.createFraudBoxes(txB) :+ newCleanerBox

    // generate tx
    val unsignedTx = txB.boxesToSpend((Seq(eventBox.getBox, cleanerBox.getBox) ++ feeBoxes).asJava)
      .fee(Configs.fee)
      .sendChangeTo(Utils.getAddress(Configs.cleaner.address))
      .outputs(outputBoxes: _*)
      .build()

    signTransaction(ctx, unsignedTx, "MoveToFraud")
  }

  /**
   * generates a transaction that mergers fraudBox to bankBox, unlocks RSN and sends it to collector address
   *
   * @param ctx blockchain context
   * @param fraudBox fraud box
   * @param bankBox bank box
   * @param cleanerBox the box contains cleanup token
   * @param feeBoxes other boxes of cleaner - empty list if cleaner box ergs is enough
   */
  def mergeFraud(ctx: BlockchainContext, fraudBox: FraudBox, bankBox: BankBox, cleanerBox: CleanerBox, feeBoxes: Seq[InputBox]): SignedTransaction = {
    val txB = ctx.newTxBuilder()

    // get UTP of fraud box and list of UTPs and EWRs in bank
    val UTP = fraudBox.getUTP
    val bankUTPs = bankBox.getUTPs
    val bankEWRs = bankBox.getEWRs
    val watcherIndex = bankUTPs.map(item => Base16.encode(item)).indexOf(Base16.encode(UTP))
    if (watcherIndex == -1) throw unexpectedException(s"watcher ${Base16.encode(UTP)} not found in bank")

    // generate bank box, remove UTP if this is the only EWR he has
    val newBankBox: OutBox = if (bankEWRs(watcherIndex) == 1) {
      val newUTPs = bankUTPs.patch(watcherIndex, Nil, 1)
      val newEWRs = bankEWRs.patch(watcherIndex, Nil, 1)
      bankBox.createBankBox(txB, newUTPs, newEWRs, watcherIndex)
    } // or reduce it by 1 if he has more EWR
    else if (bankEWRs(watcherIndex) > 1) {
      val newEWRs = bankEWRs.slice(0, watcherIndex) ++ Seq(bankEWRs(watcherIndex) - 1) ++ bankEWRs.slice(watcherIndex + 1, bankEWRs.length)
      bankBox.createBankBox(txB, bankUTPs, newEWRs, watcherIndex)
    }
    else throw unexpectedException(s"Amount of watcher's EWR is ${bankEWRs(watcherIndex)}")

    // generate cleaner box
    val newCleanerBox = cleanerBox.createCleanerBox(txB, feeBoxes)

    // generate collector boxes, form outputBoxes with new cleaner box and bank box
    val outputBoxes = Seq(newBankBox, fraudBox.createCollectorBox(txB), newCleanerBox)

    // generate tx
    val unsignedTx = txB.boxesToSpend((Seq(bankBox.getBox, fraudBox.getBox, cleanerBox.getBox) ++ feeBoxes).asJava)
      .fee(Configs.fee)
      .sendChangeTo(Utils.getAddress(Configs.cleaner.address))
      .outputs(outputBoxes: _*)
      .build()

    signTransaction(ctx, unsignedTx, "MergeFraudToBank")
  }

}
