package testUtils

import network.Client
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

trait TestSuite extends AnyPropSpec with Matchers {

  protected val client = new Client
  client.setClient()

  protected val blockchainHeight: Long = client.getHeight
  // In some test box creation height is 10k less than blockchain height. So it should be more than 10k (11k for assurance)
  assert(blockchainHeight > 11000)

}
