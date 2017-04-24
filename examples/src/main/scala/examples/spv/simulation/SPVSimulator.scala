package examples.spv.simulation

import examples.spv.Algos
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Blake2b256

object SPVSimulator extends App with ScorexLogging with SimulatorFuctions {

  val Height = 10000
  val Difficulty = BigInt(1)
  val stateRoot = Blake2b256("")
  val minerKeys = PrivateKey25519Companion.generateKeys(stateRoot)

  val genesis = genGenesisHeader(stateRoot, minerKeys._2)
  val st = System.currentTimeMillis()
  val headerChain = genChain(Height, Difficulty, stateRoot, genesis, IndexedSeq(genesis))

  val lastBlock = headerChain.last
  var minDiff = Difficulty
  lastBlock.interlinks.foreach { id =>
    println(minDiff + " => " + Algos.blockIdDifficulty(id) + " => " + (headerChain.length - headerChain.indexWhere(_.id sameElements id)))
    minDiff = minDiff * 2
  }


}
