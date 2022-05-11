package helpers

import java.math.BigInteger

import com.typesafe.config.{Config, ConfigFactory}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.{NetworkType, RestApiErgoClient}

trait ConfigHelper extends RosenLogging {
  val config: Config = ConfigFactory.load()

  /**
   * Read the config and return the value of the key
   *
   * @param key     key to find
   * @param default default value if the key is not found
   * @return value of the key
   */
  def readKey(key: String, default: String = null): String = {
    try {
      if (config.hasPath(key)) config.getString(key)
      else if (default.nonEmpty) default
      else throw new Exception(s"$key not found.")
    } catch {
      case _: Throwable =>
        log.error(s"$key is required.")
        sys.exit()
    }
  }
}

object Configs extends ConfigHelper {
  object node {
    lazy val url: String = readKey("node.url")
    lazy val networkType: NetworkType = if (readKey("node.networkType").toLowerCase.equals("mainnet")) NetworkType.MAINNET else NetworkType.TESTNET
  }
  lazy val explorer: String = readKey("explorer.url", RestApiErgoClient.getDefaultExplorerUrl(node.networkType))
  lazy val fee: Long = readKey("fee.default", "1000000").toLong
  lazy val maxFee: Long = readKey("fee.max", "1000000").toLong
  lazy val minBoxValue: Long = readKey("box.min", "100000").toLong
  lazy val addressEncoder = new ErgoAddressEncoder(node.networkType.networkPrefix)
  object tokens {
    lazy val RSN: String = readKey("tokens.RSN")
    lazy val RWT: String = readKey("tokens.RWT")
    lazy val RepoNFT: String = readKey("tokens.RepoNFT")
    lazy val GuardNFT: String = readKey("tokens.GuardNFT")
    lazy val CleanupNFT: String = readKey("tokens.CleanupNFT")
  }
  lazy val cleanupConfirm: Long = readKey("cleanup.confirm").toLong
  object cleaner {
    lazy val secret: BigInteger = BigInt(readKey("cleaner.secret"), 16).bigInteger
    lazy val address: String = readKey("cleaner.address")
    lazy val slashedAddress: String = readKey("cleaner.slashedAddress")
  }
  lazy val processInterval: Int = readKey("processInterval", "600000").toInt
}
