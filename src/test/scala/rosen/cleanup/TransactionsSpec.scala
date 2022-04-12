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

    val fraudBoxTokens = Seq(new ErgoToken(Configs.tokens.EWR, 1))

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
    outputFrauds.map(fraud => new FraudBox(fraud).getUTP) should contain theSameElementsAs eventBox.getUTPs.toSeq
  }

  /**
   * Target: testing mergeFraud
   * Dependencies:
   *    -
   * Expected Output:
   *    The function should construct a valid signedTransaction
   *    First output should be bank box
   *    Second output should be collector box, containing unlocked RSN
   *    Third output should be cleaner box
   */
  property("mergeFraud constructs valid transaction, removes UTP if it has 0 EWR") {
    // initialize test data
    val bankBox = TestBoxes.mockBankBox(5, (blockchainHeight - 10).toInt)
    val fraudBox = TestBoxes.mockFraudBox(bankBox.getUTPs.head, (blockchainHeight - 5).toInt)
    val cleanerBox = TestBoxes.mockCleanerBox(1e9.toLong, (blockchainHeight - 10).toInt)
    val feeBoxes = Seq.empty[InputBox]

    val collectorBoxTokens = Seq(new ErgoToken(Configs.tokens.RSN, 100))
    val bankBoxTokens = Seq(
      new ErgoToken(Configs.tokens.BankNft, 1),
      new ErgoToken(Configs.tokens.EWR, 101),
      new ErgoToken(Configs.tokens.RSN, 9900)
    )

    // run test
    val tx = client.getClient.execute(ctx => transactions.mergeFraud(ctx, fraudBox, bankBox, cleanerBox, feeBoxes))
    val outputBoxes = tx.getOutputsToSpend.asScala
    val outputBank = new BankBox(outputBoxes.head)
    val outputCollector = outputBoxes(1)
    val outputCleaner = outputBoxes(2)

    // verify tx size condition
    outputBoxes.length should be(4)

    // verify bank box conditions
    outputBank.getTokens should equal(bankBoxTokens)
    val newUTPs = outputBank.getUTPs
    newUTPs should equal(bankBox.getUTPs.slice(1, 5))
    val newEWRs = outputBank.getEWRs
    newEWRs should equal(bankBox.getEWRs.slice(1, 5))

    // verify collector box conditions
    outputCollector.getTokens.asScala should equal(collectorBoxTokens)
    Configs.addressEncoder.fromProposition(outputCollector.getErgoTree).get should equal(Utils.getAddress(Configs.cleaner.collectorAddress))

    // verify cleaner box conditions
    outputCleaner.getTokens.get(0) should equal(cleanerBox.getTokens.head)
  }

  /**
   * Target: testing mergeFraud
   * Dependencies:
   *    -
   * Expected Output:
   *    The function should construct a valid signedTransaction
   *    First output should be bank box
   *    Second output should be collector box, containing unlocked RSN
   *    Third output should be cleaner box
   */
  property("mergeFraud constructs valid transaction, reduces EWR by 1") {
    // initialize test data
    val bankBox = TestBoxes.mockBankBox(5, (blockchainHeight - 10).toInt)
    val fraudBox = TestBoxes.mockFraudBox(bankBox.getUTPs(1), (blockchainHeight - 5).toInt)
    val cleanerBox = TestBoxes.mockCleanerBox(1e9.toLong, (blockchainHeight - 10).toInt)
    val feeBoxes = Seq.empty[InputBox]

    val collectorBoxTokens = Seq(new ErgoToken(Configs.tokens.RSN, 100))
    val bankBoxTokens = Seq(
      new ErgoToken(Configs.tokens.BankNft, 1),
      new ErgoToken(Configs.tokens.EWR, 101),
      new ErgoToken(Configs.tokens.RSN, 9900)
    )

    // run test
    val tx = client.getClient.execute(ctx => transactions.mergeFraud(ctx, fraudBox, bankBox, cleanerBox, feeBoxes))
    val outputBoxes = tx.getOutputsToSpend.asScala
    val outputBank = new BankBox(outputBoxes.head)
    val outputCollector = outputBoxes(1)
    val outputCleaner = outputBoxes(2)

    // verify tx size condition
    outputBoxes.length should be(4)

    // verify bank box conditions
    outputBank.getTokens should equal(bankBoxTokens)
    val newUTPs = outputBank.getUTPs
    newUTPs should equal(bankBox.getUTPs)
    val newEWRs = outputBank.getEWRs
    newEWRs(1) should equal(bankBox.getEWRs(1) - 1)
    newEWRs.slice(2, 5) :+ newEWRs.head should equal(bankBox.getEWRs.slice(2, 5) :+ bankBox.getEWRs.head)

    // verify collector box conditions
    outputCollector.getTokens.asScala should equal(collectorBoxTokens)
    Configs.addressEncoder.fromProposition(outputCollector.getErgoTree).get should equal(Utils.getAddress(Configs.cleaner.collectorAddress))

    // verify cleaner box conditions
    outputCleaner.getTokens.get(0) should equal(cleanerBox.getTokens.head)
  }

}
