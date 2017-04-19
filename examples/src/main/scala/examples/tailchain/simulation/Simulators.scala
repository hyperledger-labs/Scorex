package examples.tailchain.simulation

import examples.commons.SimpleBoxTransaction
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.tailchain.core.{Algos, Constants}
import examples.tailchain.core.Constants._
import examples.tailchain.modifiers.TBlock
import examples.tailchain.utxo.PersistentAuthenticatedUtxo
import scorex.core.transaction.state.PrivateKey25519Companion

trait Simulators {


  val minerKeys = PrivateKey25519Companion.generateKeys(scorex.utils.Random.randomBytes(32))
  val minerPubKey = minerKeys._2
  val minerPrivKey = minerKeys._1

  val defaultId = Array.fill(32)(0: Byte)

  def generateBlock(txs: Seq[SimpleBoxTransaction],
                    currentUtxo: InMemoryAuthenticatedUtxo,
                    miningUtxos: IndexedSeq[InMemoryAuthenticatedUtxo]): (TBlock, Seq[PublicKey25519NoncedBox], InMemoryAuthenticatedUtxo) = {
    //todo: fix, hashchain instead of Merkle tree atm
    val txsHash = hashfn(scorex.core.utils.concatBytes(txs.map(_.bytes)))

    val changes = PersistentAuthenticatedUtxo.changes(txs).get
    val updUtxo = currentUtxo.applyChanges(changes, scorex.utils.Random.randomBytes()).get

    val h = Algos.pow(defaultId, txsHash, currentUtxo.rootHash, minerPubKey.pubKeyBytes,
      miningUtxos, Constants.Difficulty, 10000).get.get

    val newRichBoxes = txs.flatMap(_.newBoxes).filter(_.value > 5)
    (TBlock(h, txs, System.currentTimeMillis()), newRichBoxes, updUtxo)
  }

}
