package helpers

import java.math.BigInteger

import com.typesafe.config.{Config, ConfigFactory}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.{ErgoClient, NetworkType, RestApiErgoClient}

trait ConfigHelper {
  val config: Config  = ConfigFactory.load()

  /**
   * Read the config and return the value of the key
   *
   * @param key     key to find
   * @param default default value if the key is not found
   * @return value of the key
   */
  def readKey(key: String, default: String = null): String = {
    try {
      config.getString(key)
    } catch {
      case _: Throwable =>
        println(s"$key is required.")
        sys.exit()
    }
  }
}

object Configs extends ConfigHelper {
  object node {
    lazy val apiKey: String = readKey("node.apiKey")
    lazy val url: String = readKey("node.url")
    lazy val networkType: NetworkType = if (readKey("node.networkType").toLowerCase.equals("mainnet")) NetworkType.MAINNET else NetworkType.TESTNET
  }
  private lazy val explorerUrlConf = readKey("explorer.url", "")
  lazy val explorer: String = if (explorerUrlConf.isEmpty) RestApiErgoClient.getDefaultExplorerUrl(node.networkType) else explorerUrlConf
  lazy val fee: Long = readKey("fee.default", "1000000").toLong
  lazy val maxFee: Long = readKey("fee.max", "1000000").toLong
  lazy val minBoxValue: Long = readKey("box.min").toLong
  val ergoClient: ErgoClient = RestApiErgoClient.create(node.url, node.networkType, node.apiKey, explorer)
  lazy val addressEncoder = new ErgoAddressEncoder(node.networkType.networkPrefix)
  object tokens {
    lazy val RSN: String = readKey("tokens.RSN")
    lazy val EWR: String = readKey("tokens.EWR")
    lazy val BankNft: String = readKey("tokens.BankNFT")
    lazy val GuardNFT: String = readKey("tokens.GuardNFT")
    lazy val CleanupNFT: String = readKey("tokens.CleanupNFT")
  }
  lazy val cleanupConfirm: Long = readKey("cleanup.confirm").toLong
  object cleaner {
    lazy val secret: BigInteger = BigInt(readKey("cleaner.secret"), 16).bigInteger
    lazy val address: String = readKey("cleaner.address")
    lazy val collectorAddress: String = readKey("cleaner.collectorAddress")
  }
  lazy val processInterval: Int = readKey("processInterval", "600000").toInt
}
