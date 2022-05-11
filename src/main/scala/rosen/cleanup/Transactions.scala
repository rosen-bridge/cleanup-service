package rosen.cleanup

import helpers.{Configs, RosenLogging, Utils}
import helpers.RosenExceptions.{ProveException, UnexpectedException}
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
   * @param txName transaction name to log (either 'MoveToFraud' or 'SlashFraudBox')
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
        throw ProveException()
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
   * generates a transaction that redeems and slashes RSN token of fraud box and sends it to slash address
   *
   * @param ctx blockchain context
   * @param fraudBox fraud box
   * @param repoBox repo box
   * @param cleanerBox the box contains cleanup token
   * @param feeBoxes other boxes of cleaner - empty list if cleaner box ergs is enough
   */
  def slashFraud(ctx: BlockchainContext, fraudBox: FraudBox, repoBox: RWTRepoBox, cleanerBox: CleanerBox, feeBoxes: Seq[InputBox]): SignedTransaction = {
    val txB = ctx.newTxBuilder()

    // get WID of fraud box and list of WIDs and RWTs in repo
    val WID = fraudBox.getWID
    val repoWIDs = repoBox.getWIDs
    val repoRWTs = repoBox.getRWTs
    val watcherIndex = repoWIDs.map(item => Base16.encode(item)).indexOf(Base16.encode(WID))
    if (watcherIndex == -1) throw UnexpectedException(s"watcher ${Base16.encode(WID)} not found in repo")

    // generate repo box, remove WID if this is the only RWT he has
    val newRepoBox: OutBox = if (repoRWTs(watcherIndex) == 1) {
      val newWIDs = repoWIDs.patch(watcherIndex, Nil, 1)
      val newRWTs = repoRWTs.patch(watcherIndex, Nil, 1)
      repoBox.createRepoBox(txB, newWIDs, newRWTs, watcherIndex)
    } // or reduce it by 1 if he has more RWT
    else if (repoRWTs(watcherIndex) > 1) {
      val newRWTs = repoRWTs.slice(0, watcherIndex) ++ Seq(repoRWTs(watcherIndex) - 1) ++ repoRWTs.slice(watcherIndex + 1, repoRWTs.length)
      repoBox.createRepoBox(txB, repoWIDs, newRWTs, watcherIndex)
    }
    else throw UnexpectedException(s"Amount of watcher's RWT is ${repoRWTs(watcherIndex)}")

    // generate cleaner box
    val newCleanerBox = cleanerBox.createCleanerBox(txB, feeBoxes)

    // generate slashed boxes, form outputBoxes with new cleaner box and repo box
    val outputBoxes = Seq(newRepoBox, fraudBox.createSlashedBox(txB, repoBox.getRSNFactor), newCleanerBox)

    // generate tx
    val unsignedTx = txB.boxesToSpend((Seq(repoBox.getBox, fraudBox.getBox, cleanerBox.getBox) ++ feeBoxes).asJava)
      .fee(Configs.fee)
      .sendChangeTo(Utils.getAddress(Configs.cleaner.address))
      .outputs(outputBoxes: _*)
      .build()

    signTransaction(ctx, unsignedTx, "SlashFraudBox")
  }

}
