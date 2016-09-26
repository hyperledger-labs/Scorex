package examples.curvepos.transaction

import java.nio.ByteBuffer

import com.google.common.primitives.Ints
import scorex.core.NodeViewComponentCompanion
import scorex.core.block.StateChanges
import scorex.core.transaction.TransactionChanges
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.utils.ScorexLogging
import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}


class SimpleState extends ScorexLogging
  with MinimalState[PublicKey25519Proposition, PublicKey25519NoncedBox, SimpleTransaction, SimpleBlock, SimpleState] {

  private val EmptyVersion: Int = 0
  private var v: Int = EmptyVersion

  private val storage: TrieMap[ByteBuffer, PublicKey25519NoncedBox] = TrieMap()
  private val accountIndex: TrieMap[String, PublicKey25519NoncedBox] = TrieMap()

  def isEmpty: Boolean = v == EmptyVersion

  override def version: VersionTag = Ints.toByteArray(v)

  override def accountBox(p: PublicKey25519Proposition): Seq[PublicKey25519NoncedBox] =
    accountIndex.get(p.address).map(box => Seq(box)).getOrElse(Seq())

  override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] =
    storage.get(ByteBuffer.wrap(boxId))

  override def rollbackTo(version: VersionTag): Try[SimpleState] = {
    log.warn("Rollback is not implemented")
    Try(this)
  }


  override def applyChanges(change: StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]): Try[SimpleState] = Try {
    change.toRemove.foreach(b => storage.remove(ByteBuffer.wrap(b.id)))
    change.toAppend.foreach { b =>
      storage.put(ByteBuffer.wrap(b.id), b)
      accountIndex.put(b.proposition.address, b)
    }
    this
  }

  override def applyChanges(block: SimpleBlock): Try[SimpleState] = Try {
    val generatorReward = block.txs.map(_.fee).sum
    val gen = PublicKey25519Proposition(block.generator)
    val generatorBox: PublicKey25519NoncedBox = accountBox(gen).headOption match {
      case Some(oldBox) => oldBox.copy(nonce = oldBox.nonce + 1, value = oldBox.value + generatorReward)
      case None => PublicKey25519NoncedBox(gen, 1, generatorReward)
    }

    val txChanges = block.txs.map(tx => changes(tx)).map(_.get)
    val toRemove: Set[PublicKey25519NoncedBox] = txChanges.flatMap(_.toRemove).toSet
    val toAppend: Set[PublicKey25519NoncedBox] = (generatorBox +: txChanges.flatMap(_.toAppend)).toSet
    applyChanges(StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](toRemove, toAppend)).get
  }

  override def companion: NodeViewComponentCompanion = ???

  override type NVCT = this.type

  override def validate(transaction: SimpleTransaction): Try[Unit] = transaction match {
    case sp: SimplePayment => Try {
      val b = accountBox(PublicKey25519Proposition(sp.sender)).head
      (b.value >= sp.amount + sp.fee) && (b.nonce + 1 == sp.nonce)
    }
  }

  /**
    * A Transaction opens existing boxes and creates new ones
    */
  override def changes(transaction: SimpleTransaction): Try[TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
  transaction match {
    case _: SimplePayment => Failure(new Exception("implementation is needed"))
  }
}
