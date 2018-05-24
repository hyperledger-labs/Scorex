package examples.trimchain.simulation

import examples.commons.{PublicKey25519NoncedBox, SimpleBoxTransaction}
import examples.trimchain.core.Constants._
import examples.trimchain.core.{Algos, Constants, StateRoot, TransactionsRoot}
import examples.trimchain.modifiers.TBlock
import examples.trimchain.utxo.PersistentAuthenticatedUtxo
import scorex.core._
import scorex.core.transaction.state.PrivateKey25519Companion

trait Simulators {


  val minerKeys = PrivateKey25519Companion.generateKeys(scorex.utils.Random.randomBytes(32))
  val minerPubKey = minerKeys._2
  val minerPrivKey = minerKeys._1

  val defaultId = VersionTag @@ (ModifierId @@ Array.fill(32)(0: Byte))

  def generateBlock(txs: Seq[SimpleBoxTransaction],
                    currentUtxo: InMemoryAuthenticatedUtxo,
                    miningUtxos: IndexedSeq[InMemoryAuthenticatedUtxo]): (TBlock, Seq[PublicKey25519NoncedBox], InMemoryAuthenticatedUtxo) = {
    //todo: fix, hashchain instead of Merkle tree atm
    val txsHash = TransactionsRoot @@ hashfn(scorex.core.utils.concatBytes(txs.map(_.bytes)))

    val changes = PersistentAuthenticatedUtxo.changes(txs).get
    val updUtxo = currentUtxo.applyChanges(changes, VersionTag @@ scorex.utils.Random.randomBytes()).get

    // TODO: review me - .get.get
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val h = Algos.pow(defaultId, txsHash, StateRoot @@ currentUtxo.rootHash, minerPubKey.pubKeyBytes,
      miningUtxos, Constants.Difficulty, 10000).get.get

    val newRichBoxes = txs.flatMap(_.newBoxes).filter(_.value > 5)
    (TBlock(h, txs, System.currentTimeMillis()), newRichBoxes, updUtxo)
  }

}
