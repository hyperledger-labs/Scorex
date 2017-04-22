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


  val Height = 10000
  val Difficulty = BigInt(1)
  val stateRoot = Blake2b256("")
  val minerKeys = PrivateKey25519Companion.generateKeys(stateRoot)

  val genesis = genGenesisHeader(stateRoot, minerKeys._2)
  val blockchain = genChain(Height, Difficulty, stateRoot, IndexedSeq(genesis))
  val lastBlock = blockchain.last

  property("SPVSimulator generate chain starting from genesis") {
    blockchain.head shouldBe genesis
  }

  property("Last block interlinks are correct") {
    val interLinks = lastBlock.innerchainLinks

    var currentDifficulty = Difficulty
    interLinks.foreach { id =>
      println(blockIdDifficulty(id) + " => " + currentDifficulty)
      blockIdDifficulty(id) should be >= currentDifficulty
      currentDifficulty = currentDifficulty * 2
    }
  }


}
