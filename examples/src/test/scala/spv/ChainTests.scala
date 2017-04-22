package spv

import examples.spv.simulation.SimulatorFuctions
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.hash.Blake2b256


class ChainTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with SPVGenerators
  with SimulatorFuctions {


  val height = 1000
  val stateRoot = Blake2b256("")
  val minerKeys = PrivateKey25519Companion.generateKeys(stateRoot)

  val genesis = genGenesisHeader(stateRoot, minerKeys._2)
  val blockchain = genChain(height, stateRoot, Seq(genesis))
  val lastBlock = blockchain.head

  property("SPVSimulator generate chain starting from genesis") {
    blockchain.head shouldBe genesis
  }


}
