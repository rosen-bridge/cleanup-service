package rosen.cleanup

import helpers.Configs
import org.ergoplatform.appkit.ErgoToken
import testUtils.{TestBoxes, TestSuite}
import models._

import scala.collection.JavaConverters._

class TransactionsSpec extends TestSuite {

  private val transactions = new Transactions

  /**
   * Target: testing generateFrauds
   * Dependencies:
   *    Boxes
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

    val newCleanerFraudToken = {
      val token = cleanerBox.getTokens(1)
      new ErgoToken(token.getId, token.getValue - 5)
    }
    val fraudBoxTokens = Seq(
      new ErgoToken(Configs.tokens.EWR, 1),
      new ErgoToken(cleanerBox.getTokens(1).getId, 1)
    )

    // run test
    val tx = client.getClient.execute(ctx => transactions.generateFrauds(ctx, eventBox, cleanerBox, feeBoxes))
    val outputBoxes = tx.getOutputsToSpend.asScala
    val outputCleaner = outputBoxes(outputBoxes.length - 2)
    val outputFrauds = outputBoxes.slice(0, outputBoxes.length - 2)

    // verify tx size condition
    outputBoxes.length should be(7)

    // verify cleaner box conditions
    outputCleaner.getTokens.get(0) should equal(cleanerBox.getTokens.head)
    outputCleaner.getTokens.get(1) should equal(newCleanerFraudToken)

    // verify fraud boxes conditions
    outputFrauds.foreach(fraud => fraud.getTokens.asScala should equal(fraudBoxTokens))
    outputFrauds.map(fraud => new FraudBox(fraud).getUTP) should contain theSameElementsAs eventBox.getUTPs.toSeq
  }

}
