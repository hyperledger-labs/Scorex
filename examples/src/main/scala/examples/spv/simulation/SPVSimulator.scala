package examples.spv.simulation

import examples.spv.{Header, KMZProofSerializer, SpvAlgos}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash
import scorex.crypto.hash.Blake2b256

object SPVSimulator extends App with ScorexLogging with SimulatorFuctions {

  private val Height = 500000
  private val Difficulty = BigInt(1)
  private val stateRoot = Blake2b256("")
  private val minerKeys = PrivateKey25519Companion.generateKeys(stateRoot)

  private val genesis = genGenesisHeader(stateRoot, minerKeys._2)
  val st: Long = System.currentTimeMillis()
  private val headerChain: Seq[Header] = genChain(Height, Difficulty, stateRoot, IndexedSeq(genesis))

  val lastBlock: Option[Header] = headerChain.lastOption
  var minDiff: BigInt = Difficulty

  private val k = 6

  println(s"Chain of length $Height, k=$k")
  println("m,proofLength,blockNum,uniqueBlockNum")

  Seq(6, 15, 30, 50, 100, 127) foreach { m =>
    val proof = SpvAlgos.constructKMZProof(m, k, headerChain).get
    proof.valid.get
    val blocks:Seq[Header] = proof.suffix ++ proof.prefixProofs.flatten
    val blockNum = blocks.length
    val uniqueBlockNum = blocks.map(_.encodedId).toSet.size
    println( m + "," + KMZProofSerializer.toBytes(proof).length + "," + blockNum + "," + uniqueBlockNum)
  }


}
