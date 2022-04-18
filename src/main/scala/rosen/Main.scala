package rosen

import helpers.{Configs, RosenLogging}
import network.Client
import rosen.cleanup.{Procedures, Transactions}


object Main extends RosenLogging {

  def main(args: Array[String]): Unit = {
    // initializing network client
    val client = new Client

    // initializing service classes
    val transactions = new Transactions
    val procedures = new Procedures(client, transactions)
    log.info(s"Service initiated")

    while (true) {
      try {
        log.info(s"Processing Trigger event boxes")
        client.getClient.execute(ctx => procedures.processEvents(ctx))
      }
      catch {
        case e: Throwable => log.error(s"An error occurred while processing Trigger event boxes. Error: ${e.getMessage}")
      }

      try {
        log.info(s"Processing Fraud boxes")
        client.getClient.execute(ctx => procedures.processFrauds(ctx))
      }
      catch {
        case e: Throwable => log.error(s"An error occurred while processing Fraud boxes. Error: ${e.getMessage}")
      }

      Thread.sleep(Configs.processInterval)
    }
  }

}
