package examples.curvepos.transaction

import java.nio.ByteBuffer

import scorex.core.NodeViewComponentCompanion
import scorex.core.block.StateChanges
import scorex.core.transaction.TransactionChanges
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.utils.ScorexLogging
import scala.util.{Failure, Try}

import SimpleState.EmptyVersion

case class SimpleState(override val version: VersionTag = EmptyVersion,
                       storage: Map[ByteBuffer, PublicKey25519NoncedBox] = Map()) extends ScorexLogging
  with MinimalState[PublicKey25519Proposition, PublicKey25519NoncedBox, SimpleTransaction, SimpleBlock, SimpleState] {

  def isEmpty: Boolean = version sameElements EmptyVersion

  override def boxOf(p: PublicKey25519Proposition): Seq[PublicKey25519NoncedBox] =
    storage.values.filter(b => b.proposition.address == p.address).toSeq

  override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] =
    storage.get(ByteBuffer.wrap(boxId))

  override def rollbackTo(version: VersionTag): Try[SimpleState] = {
    log.warn("Rollback is not implemented")
    Try(this)
  }

  override def applyChanges(change: StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox], newVersion: VersionTag): Try[SimpleState] = Try {
    val rmap = change.toRemove.foldLeft(storage) { case (m, b) => m - ByteBuffer.wrap(b.id) }

    val amap = change.toAppend.foldLeft(rmap) { case (m, b) =>
      m + (ByteBuffer.wrap(b.id) -> b)
    }
    SimpleState(newVersion, amap)
  }

  override def companion: NodeViewComponentCompanion = ???

  override type NVCT = SimpleState

  override def validate(transaction: SimpleTransaction): Try[Unit] = transaction match {
    case sp: SimplePayment => Try {
      val b = boxOf(sp.sender).head
      (b.value >= sp.amount + sp.fee) && (b.nonce + 1 == sp.nonce)
    }
  }

  /**
    * A Transaction opens existing boxes and creates new ones
    */
  override def changes(transaction: SimpleTransaction): Try[TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
    transaction match {
      case _: SimplePayment => Failure(new Exception("implementation is needed"))
    }
  }

  override def changes(block: SimpleBlock): Try[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = Try {
    val generatorReward = block.txs.map(_.fee).sum
    val gen = block.generator
    val generatorBox: PublicKey25519NoncedBox = boxOf(gen).headOption match {
      case Some(oldBox) => oldBox.copy(nonce = oldBox.nonce + 1, value = oldBox.value + generatorReward)
      case None => PublicKey25519NoncedBox(gen, 1, generatorReward)
    }

    val txChanges = block.txs.map(tx => changes(tx)).map(_.get)
    val toRemove: Set[PublicKey25519NoncedBox] = txChanges.flatMap(_.toRemove).toSet
    val toAppend: Set[PublicKey25519NoncedBox] = (generatorBox +: txChanges.flatMap(_.toAppend)).toSet
    StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](toRemove, toAppend)
  }
}

object SimpleState {
  val EmptyVersion: Array[Byte] = Array.fill(32)(0: Byte)
}