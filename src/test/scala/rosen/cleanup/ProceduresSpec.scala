package rosen.cleanup

import helpers.Configs
import helpers.RosenExceptions.notEnoughErgException
import models.{BankBox, CleanerBox}
import network.Client
import org.ergoplatform.appkit.{BlockchainContext, SignedTransaction}
import org.mockito.Mockito
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{mock, verify, when}
import org.mockito.invocation.InvocationOnMock
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
    var cleanerBoxIds = Seq.empty[String]

    // mock dependencies
    val mockedTransactions = mock(classOf[Transactions])
    when(mockedTransactions.generateFrauds(any(), any(), any(), any())).thenAnswer((invocation: InvocationOnMock) => {
      val cleanerBoxParameter = invocation.getArgument(2, classOf[CleanerBox])
      cleanerBoxIds = cleanerBoxIds :+ cleanerBoxParameter.getId
      TestBoxes.mockMoveToFraudTransaction(cleanerBox, watchersLen)
    })
    val mockedClient = mock(classOf[Client])
    when(mockedClient.getHeight).thenReturn(blockchainHeight)
    when(mockedClient.getCleanerBox).thenReturn(cleanerBox.getBox)
    when(mockedClient.getEventBoxes).thenReturn(eventBoxes.map(_.getBox))
    val mockedCtx = mock(classOf[BlockchainContext])
    when(mockedCtx.sendTransaction(any()))
      .thenAnswer((invocation: InvocationOnMock) => invocation.getArgument(0, classOf[SignedTransaction]).getId)

    // run test
    val procedures = new Procedures(mockedClient, mockedTransactions)
    procedures.processEvents(mockedCtx)

    // verify generateFrauds method calls and different boxes
    verify(mockedTransactions, Mockito.times(2)).generateFrauds(any(), any(), any(), any())
    cleanerBoxIds.head should not equal(cleanerBoxIds.last)
  }

  /**
   * Target: testing processFrauds and mergeFraudToBank
   * Dependencies:
   *    Client
   *    Transaction
   * Expected Output:
   *    The function should chain two mergeFraudToBank transaction successfully
   */
  property("processFrauds chains mergeFraudToBank for two events") {
    // initialize test data
    val cleanerBox = TestBoxes.mockCleanerBox(Configs.minBoxValue + 2 * Configs.fee, (blockchainHeight - 10).toInt)
    val bankBox = TestBoxes.mockBankBox(5, (blockchainHeight - 20).toInt)
    val fraudBoxes = Seq(
      TestBoxes.mockFraudBox(bankBox.getUTPs.last, (blockchainHeight - 3).toInt),
      TestBoxes.mockFraudBox(bankBox.getUTPs.last, (blockchainHeight - 3).toInt)
    )
    var bankBoxIds = Seq.empty[String]
    var cleanerBoxIds = Seq.empty[String]

    // mock dependencies
    val mockedTransactions = mock(classOf[Transactions])
    when(mockedTransactions.mergeFraud(any(), any(), any(), any(), any())).thenAnswer((invocation: InvocationOnMock) => {
      val bankBoxParameter = invocation.getArgument(2, classOf[BankBox])
      val cleanerBoxParameter = invocation.getArgument(3, classOf[CleanerBox])
      bankBoxIds = bankBoxIds :+ bankBoxParameter.getId
      cleanerBoxIds = cleanerBoxIds :+ cleanerBoxParameter.getId
      TestBoxes.mockMergeFraudTransaction(bankBoxParameter, cleanerBoxParameter)
    })
    val mockedClient = mock(classOf[Client])
    when(mockedClient.getHeight).thenReturn(blockchainHeight)
    when(mockedClient.getCleanerBox).thenReturn(cleanerBox.getBox)
    when(mockedClient.getBankBox).thenReturn(bankBox.getBox)
    when(mockedClient.getFraudBoxes).thenReturn(fraudBoxes.map(_.getBox))
    val mockedCtx = mock(classOf[BlockchainContext])
    when(mockedCtx.sendTransaction(any()))
      .thenAnswer((invocation: InvocationOnMock) => invocation.getArgument(0, classOf[SignedTransaction]).getId)

    // run test
    val procedures = new Procedures(mockedClient, mockedTransactions)
    procedures.processFrauds(mockedCtx)

    // verify mergeFraud method calls and different boxes
    verify(mockedTransactions, Mockito.times(2)).mergeFraud(any(), any(), any(), any(), any())
    bankBoxIds.head should not equal(bankBoxIds.last)
    cleanerBoxIds.head should not equal(cleanerBoxIds.last)
  }

}
