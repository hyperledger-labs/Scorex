package examples.spv.simulation

import examples.spv.{Algos, KLS16ProofSerializer}
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Blake2b256

object SPVSimulator extends App with ScorexLogging with SimulatorFuctions {

  val Height = 5000
  val Difficulty = BigInt(1)
  val stateRoot = Blake2b256("")
  val minerKeys = PrivateKey25519Companion.generateKeys(stateRoot)

  val genesis = genGenesisHeader(stateRoot, minerKeys._2)
  val st = System.currentTimeMillis()
  val headerChain = genChain(Height, Difficulty, stateRoot, IndexedSeq(genesis))

  val lastBlock = headerChain.last
  var minDiff = Difficulty
  lastBlock.interlinks.foreach { id =>

    println(minDiff + " => " + Algos.blockIdDifficulty(id) + " => " +
      (headerChain.length - headerChain.indexWhere(_.id sameElements id)))

    minDiff = minDiff * 2
  }
  println(lastBlock)

  val k = 5

  (1 to 20) foreach { m =>
    val proof = Algos.constructKLS16Proof(m, k, headerChain).get
    proof.validate.get
    println(m + " => " + KLS16ProofSerializer.toBytes(proof))
  }

}
