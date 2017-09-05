package examples.trimchain.simulation

import com.google.common.primitives.{Ints, Longs}
import examples.curvepos.{Nonce, Value}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.trimchain.core.Constants._
import examples.trimchain.core.{Algos, Constants}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{BoxStateChanges, Insertion}

object ValidationSimulator extends App with Simulators {

  // NElementsInProof: 2, validation time: 0.000434440 seconds
  // NElementsInProof: 5, validation time: 0.000532366 seconds
  // NElementsInProof: 10, validation time: 0.000530379 seconds
  // NElementsInProof: 20, validation time: 0.000618186 seconds

  val StateSize = 5000000
  val ValidationNum = 10000

  val genesisBoxes = (1 to StateSize) map { i =>
    PublicKey25519NoncedBox(
      minerPubKey,
      Nonce @@ Longs.fromByteArray(hashfn(minerPubKey.pubKeyBytes ++ Ints.toByteArray(i)).take(8)),
      Value @@ 10000000000L
    )
  }

  val genesisChanges: BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox] =
    BoxStateChanges(genesisBoxes.map(box => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](box)))

  val genesisUtxo = InMemoryAuthenticatedUtxo(genesisBoxes.size, None, defaultId).applyChanges(genesisChanges, defaultId).get
  val rootHash = genesisUtxo.rootHash

  var validationTime: Long = 0L
  (0 until ValidationNum) foreach { _ =>
    val block = generateBlock(Seq(), genesisUtxo, IndexedSeq(genesisUtxo))._1
    val start = System.nanoTime()
    Algos.validatePow(block.header, IndexedSeq(rootHash), Constants.Difficulty)
    println("!! " + (System.nanoTime() - start))
    validationTime += System.nanoTime() - start
  }
  println(s"One block validation takes ${validationTime / ValidationNum} nanos")


}
