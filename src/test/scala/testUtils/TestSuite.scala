package testUtils

import network.Client
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

trait TestSuite extends AnyPropSpec with Matchers {

  protected val client = new Client
  client.setClient()

  protected val blockchainHeight: Long = client.getHeight

}
