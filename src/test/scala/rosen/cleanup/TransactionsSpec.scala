package rosen.cleanup

import helpers.{Configs, Utils}
import org.ergoplatform.appkit.{ErgoToken, InputBox}
import testUtils.{TestBoxes, TestSuite}
import models._

import scala.collection.JavaConverters._

class TransactionsSpec extends TestSuite {

  private val transactions = new Transactions

  /**
   * Target: testing generateFrauds
   * Dependencies:
   *    -
   * Expected Output:
   *    The function should construct a valid signedTransaction
   *    First output should be cleanerBox
   *    All other outputs (except fee box) should be fraudBox
   */
  property("generateFrauds constructs valid transaction") {
    // initialize test data
    val eventBox = TestBoxes.mockTriggerEventBox(5, (blockchainHeight - Configs.cleanupConfirm - 1).toInt)
    val cleanerBox = TestBoxes.mockCleanerBox(Configs.minBoxValue, (blockchainHeight - 10000).toInt)
    val feeBoxes = TestBoxes.mockFeeBoxes((blockchainHeight - 9000).toInt)

    val fraudBoxTokens = Seq(new ErgoToken(Configs.tokens.RWT, 100))

    // run test
    val tx = client.getClient.execute(ctx => transactions.generateFrauds(ctx, eventBox, cleanerBox, feeBoxes))
    val outputBoxes = tx.getOutputsToSpend.asScala
    val outputCleaner = outputBoxes(outputBoxes.length - 2)
    val outputFrauds = outputBoxes.slice(0, outputBoxes.length - 2)

    // verify tx size condition
    outputBoxes.length should be(7)

    // verify cleaner box conditions
    outputCleaner.getTokens.get(0) should equal(cleanerBox.getTokens.head)

    // verify fraud boxes conditions
    outputFrauds.foreach(fraud => fraud.getTokens.asScala should equal(fraudBoxTokens))
    outputFrauds.map(fraud => new FraudBox(fraud).getWID) should contain theSameElementsAs eventBox.getWIDs.toSeq
  }

  /**
   * Target: testing slashFraud
   * Dependencies:
   *    -
   * Expected Output:
   *    The function should construct a valid signedTransaction
   *    First output should be repo box
   *    Second output should be slashed box, containing unlocked RSN
   *    Third output should be cleaner box
   */
  property("slashFraud constructs valid transaction, slashing RSNs corresponding to the fraud RWTs") {
    // initialize test data
    val repoBox = TestBoxes.mockRepoBox(5, (blockchainHeight - 10).toInt)
    val fraudBox = TestBoxes.mockFraudBox(repoBox.getWIDs(1), (blockchainHeight - 5).toInt)
    val cleanerBox = TestBoxes.mockCleanerBox(1e9.toLong, (blockchainHeight - 10).toInt)
    val feeBoxes = Seq.empty[InputBox]

    val slashedBoxTokens = Seq(new ErgoToken(Configs.tokens.RSN, 100))
    val repoBoxTokens = Seq(
      new ErgoToken(Configs.tokens.RepoNFT, 1),
      new ErgoToken(Configs.tokens.RWT, 200),
      new ErgoToken(Configs.tokens.RSN, 9900)
    )

    // run test
    val tx = client.getClient.execute(ctx => transactions.slashFraud(ctx, fraudBox, repoBox, cleanerBox, feeBoxes))
    val outputBoxes = tx.getOutputsToSpend.asScala
    val outputRepo = new RWTRepoBox(outputBoxes.head)
    val outputSlashed = outputBoxes(1)
    val outputCleaner = outputBoxes(2)

    // verify tx size condition
    outputBoxes.length should be(4)

    // verify repo box conditions
    outputRepo.getTokens should equal(repoBoxTokens)
    val newWIDs = outputRepo.getWIDs
    newWIDs should equal(repoBox.getWIDs)
    val newRWTs = outputRepo.getRWTs
    newRWTs(1) should equal(repoBox.getRWTs(1) - fraudBox.getTokens.head.getValue)
    newRWTs.slice(2, 5) :+ newRWTs.head should equal(repoBox.getRWTs.slice(2, 5) :+ repoBox.getRWTs.head)

    // verify slashed box conditions
    outputSlashed.getTokens.asScala should equal(slashedBoxTokens)
    Configs.addressEncoder.fromProposition(outputSlashed.getErgoTree).get should equal(Utils.getAddress(Configs.cleaner.slashedAddress))

    // verify cleaner box conditions
    outputCleaner.getTokens.get(0) should equal(cleanerBox.getTokens.head)
  }

}
