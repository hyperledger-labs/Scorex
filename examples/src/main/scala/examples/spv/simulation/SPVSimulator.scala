package examples.spv.simulation

import examples.spv.Header
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.utils.ScorexLogging

object SPVSimulator extends App with ScorexLogging with SimulatorFuctions {

  val Difficulty = BigInt(1000)
  val minerKeys = PrivateKey25519Companion.generateKeys(scorex.utils.Random.randomBytes(32))

  val genesisUtxo = genGenesisState(minerKeys._2)

  val genesisHeader: Header = genGenesisHeader(genesisUtxo.rootHash, minerKeys._2)

  val headerChain = genChain(100, Difficulty, genesisUtxo.rootHash, Seq(genesisHeader))
  headerChain.foreach(println)

  val lastBlock = headerChain.last
  var minDiff = Difficulty
  lastBlock.innerchainLinks.foreach { id =>
    println(minDiff + " => " + blockIdDifficulty(id))
    minDiff = minDiff * 2
  }

}
