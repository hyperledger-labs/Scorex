package examples.spv.simulation

import examples.spv.{Algos, KMZProofSerializer}
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

  val k = 6

  println("Chain constructed!")

  Seq(6, 15, 30, 50, 100, 127) foreach { m =>
    val proof = Algos.constructKMZProof(m, k, headerChain).get
    proof.valid.get
    val blockNum = proof.suffix.length + proof.prefixProofs.flatten.length
    println( m + " => " + KMZProofSerializer.toBytes(proof).length + "," + blockNum)
  }


}
