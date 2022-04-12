package helpers

import org.ergoplatform.ErgoAddress
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, ErgoContract}

object Utils {

  /**
   * converts Address to ErgoContract
   */
  def generateAddress(contract: ErgoContract): Address = {
    Address.create(Configs.addressEncoder.fromProposition(contract.getErgoTree).get.toString)
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

  /**
   * converts ErgoContract to base58 string representation of address
   */
  def getContractAddress(contract: ErgoContract): String = {
    val ergoTree = contract.getErgoTree
    Configs.addressEncoder.fromProposition(ergoTree).get.toString
  }

}
