package rosen.cleanup

import helpers.Configs
import helpers.RosenExceptions.notEnoughErgException
import network.Client
import org.ergoplatform.appkit.{BlockchainContext, ErgoToken}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{mock, when}
import org.scalatest.PrivateMethodTester
import testUtils.{TestBoxes, TestSuite}

class ProceduresSpec extends TestSuite with PrivateMethodTester  {

  /**
   * Target: testing moveToFraud
   * Dependencies:
   *    Client
   *    Transaction
   * Expected Output:
   *    The function should abort process when there's not enough erg
   */
  property("moveToFraud aborts process when there's not enough erg") {
    // initialize test data
    val eventBox = TestBoxes.mockTriggerEventBox(3, (blockchainHeight - Configs.cleanupConfirm - 1).toInt)
    val cleanerBox = TestBoxes.mockCleanerBox(Configs.minBoxValue, (blockchainHeight - 10000).toInt)
    val feeBoxes = Seq(TestBoxes.mockMinimumFeeBox((blockchainHeight - 3000).toInt))

    // mock dependencies
    val mockedTransactions = mock(classOf[Transactions])
    val mockedClient = mock(classOf[Client])
    when(mockedClient.getCleanerBox).thenReturn(cleanerBox.getBox)
    when(mockedClient.getCleanerFeeBoxes).thenReturn(feeBoxes)

    // run test
    val moveToFraudMethod = PrivateMethod[Unit]('moveToFraud)
    val procedures = new Procedures(mockedClient, mockedTransactions)
    try {
      client.getClient.execute(ctx => procedures invokePrivate moveToFraudMethod(ctx, eventBox))
      fail("No exception threw")
    }
    catch {
      case _: notEnoughErgException => succeed
      case e: Throwable => fail(s"Wrong exception: ${e.getMessage}")
    }
  }

  /**
   * Target: testing processEvents and moveToFraud
   * Dependencies:
   *    Client
   *    Transaction
   * Expected Output:
   *    The function should chain two MoveToFraud transaction successfully
   */
  property("processEvents chains MoveToFraud for two events") {
    // initialize test data
    val cleanerBox = TestBoxes.mockCleanerBox(Configs.minBoxValue + 2 * Configs.fee, (blockchainHeight - 10000).toInt)
    val eventBoxes = Seq(
      TestBoxes.mockTriggerEventBox(3, (blockchainHeight - Configs.cleanupConfirm - 3).toInt),
      TestBoxes.mockTriggerEventBox(5, (blockchainHeight - Configs.cleanupConfirm - 1).toInt)
    )
    val watchersLen = eventBoxes.map(_.getWatchersLen).sum

    val newCleanerFraudToken = {
      val token = cleanerBox.getTokens(1)
      new ErgoToken(token.getId, token.getValue - watchersLen)
    }

    // mock dependencies
    val mockedTransactions = mock(classOf[Transactions])
    val mockedSignedTransaction = TestBoxes.mockMoveToFraudTransaction(cleanerBox, watchersLen)
    when(mockedTransactions.generateFrauds(any(), any(), any(), any())).thenReturn(mockedSignedTransaction)
    val mockedClient = mock(classOf[Client])
    when(mockedClient.getHeight).thenReturn(blockchainHeight)
    when(mockedClient.getCleanerBox).thenReturn(cleanerBox.getBox)
    when(mockedClient.getEventBoxes).thenReturn(eventBoxes.map(_.getBox))
    val mockedCtx = mock(classOf[BlockchainContext])
    val mockedTxId = TestBoxes.generateRandomId
    when(mockedCtx.sendTransaction(any())).thenReturn(mockedTxId)

    // run test
    val procedures = new Procedures(mockedClient, mockedTransactions)
    procedures.processEvents(mockedCtx)

    // verify cleaner box conditions
    val outputCleaner = procedures.getCleanerBox
    outputCleaner.getTokens.head should equal(cleanerBox.getTokens.head)
    outputCleaner.getTokens(1) should equal(newCleanerFraudToken)
  }

}
