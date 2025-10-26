package helpers

import org.ergoplatform.ErgoAddress
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, ErgoContract}

object Utils {

  /**
   * converts ErgoContract to Address
   */
  def generateAddress(contract: ErgoContract): Address = {
    Address.fromErgoTree(contract.getErgoTree, Configs.node.networkType)
  }

  /**
   * converts base58 string representation of address to ErgoTreeContract
   */
  def getAddressContract(address: String): ErgoTreeContract = {
    new ErgoTreeContract(getAddress(address).script, Configs.node.networkType)
  }

  /**
   * converts base58 string representation of address to ErgoAddress
   */
  def getAddress(address: String): ErgoAddress = {
    Configs.addressEncoder.fromString(address).get
  }

}
