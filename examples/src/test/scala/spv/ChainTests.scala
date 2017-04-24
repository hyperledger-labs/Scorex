package spv

import examples.spv.Algos
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


  val Height = 50000
  val Difficulty = BigInt(1)
  val stateRoot = Blake2b256("")
  val minerKeys = PrivateKey25519Companion.generateKeys(stateRoot)

  val genesis = genGenesisHeader(stateRoot, minerKeys._2)
//  val st = System.currentTimeMillis()
  val blockchain = genChain(Height, Difficulty, stateRoot, genesis, IndexedSeq(genesis))
//  println(System.currentTimeMillis() - st)
  val lastBlock = blockchain.last
  val lastInnerLinks = lastBlock.interlinks

  property("SPVSimulator generate chain starting from genesis") {
    blockchain.head shouldBe genesis
  }

  property("First innerchain links is to genesis") {
    lastInnerLinks.head shouldEqual genesis.id
  }

  property("Last block interlinks are correct") {
    var currentDifficulty = Difficulty
    lastInnerLinks.length should be > 1
    lastInnerLinks.tail.foreach { id =>
      println(Algos.blockIdDifficulty(id) + " >= " + currentDifficulty)
      Algos.blockIdDifficulty(id) should be >= currentDifficulty
      currentDifficulty = currentDifficulty * 2
    }
  }

}
