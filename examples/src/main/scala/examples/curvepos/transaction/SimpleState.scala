package examples.curvepos.transaction

import java.nio.ByteBuffer

import examples.curvepos.transaction.SimpleState.EmptyVersion
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{Proposition, PublicKey25519Proposition}
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.{Insertion, MinimalState, Removal, StateChanges}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.{Failure, Try}

case class TransactionChanges[P <: Proposition, BX <: Box[P, Long]](toRemove: Set[BX], toAppend: Set[BX], minerReward: Long)

case class SimpleState(override val version: VersionTag = EmptyVersion,
                       storage: Map[ByteBuffer, PublicKey25519NoncedBox] = Map()) extends ScorexLogging
  with MinimalState[Long, PublicKey25519Proposition, PublicKey25519NoncedBox, SimpleTransaction, SimpleBlock, SimpleState] {

  def isEmpty: Boolean = version sameElements EmptyVersion

  def totalBalance: Long = storage.keySet.flatMap(k => storage.get(k).map(_.value)).sum

  override def toString: String = {
    s"SimpleState at ${Base58.encode(version)}\n" + storage.keySet.flatMap(k => storage.get(k)).mkString("\n  ")
  }

  override def boxesOf(p: PublicKey25519Proposition): Seq[PublicKey25519NoncedBox] =
    storage.values.filter(b => b.proposition.address == p.address).toSeq

  override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] =
    storage.get(ByteBuffer.wrap(boxId))

  override def rollbackTo(version: VersionTag): Try[SimpleState] = {
    log.warn("Rollback is not implemented")
    Try(this)
  }

  override def applyChanges(change: StateChanges[Long, PublicKey25519Proposition, PublicKey25519NoncedBox], newVersion: VersionTag): Try[SimpleState] = Try {
    val rmap = change.toRemove.foldLeft(storage) { case (m, r) => m - ByteBuffer.wrap(r.boxId) }

    val amap = change.toAppend.foldLeft(rmap) { case (m, a) =>
      val b = a.box
      assert(b.value >= 0)
      m + (ByteBuffer.wrap(b.id) -> b)
    }
    SimpleState(newVersion, amap)
  }

  override type NVCT = SimpleState

  override def validate(transaction: SimpleTransaction): Try[Unit] = transaction match {
    case sp: SimplePayment => Try {
      val b = boxesOf(sp.sender).head
      (b.value >= Math.addExact(sp.amount, sp.fee)) && (b.nonce + 1 == sp.nonce)
    }
  }

  /**
    * A Transaction opens existing boxes and creates new ones
    */
  def changes(transaction: SimpleTransaction): Try[TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
    transaction match {
      case tx: SimplePayment if !isEmpty => Try {
        val oldSenderBox = boxesOf(tx.sender).head
        val oldRecipientBox = boxesOf(tx.recipient).headOption
        val newRecipientBox = oldRecipientBox.map { oldB =>
          oldB.copy(nonce = oldB.nonce + 1, value = Math.addExact(oldB.value, tx.amount))
        }.getOrElse(PublicKey25519NoncedBox(tx.recipient, 0L, tx.amount))
        val newSenderBox = oldSenderBox.copy(nonce = oldSenderBox.nonce + 1,
          value = Math.addExact(Math.addExact(oldSenderBox.value, -tx.amount), -tx.fee))
        val toRemove = Set(oldSenderBox) ++ oldRecipientBox
        val toAppend = Set(newRecipientBox, newSenderBox).ensuring(_.forall(_.value >= 0))

        TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](toRemove, toAppend, tx.fee)
      }
      case genesis: SimplePayment if isEmpty => Try {
        val toAppend: Set[PublicKey25519NoncedBox] = Set(PublicKey25519NoncedBox(genesis.recipient, 0L, genesis.amount))
        TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](Set(), toAppend, 0)
      }
      case _ => Failure(new Exception("implementation is needed"))
    }
  }

  override def changes(block: SimpleBlock): Try[StateChanges[Long, PublicKey25519Proposition, PublicKey25519NoncedBox]] = Try {
    val generatorReward = block.txs.map(_.fee).sum
    val gen = block.generator

    val txChanges = block.txs.map(tx => changes(tx)).map(_.get)
    val toRemove = txChanges.flatMap(_.toRemove).map(_.id).map(id =>
      Removal[Long, PublicKey25519Proposition, PublicKey25519NoncedBox](id))
    val toAppendFrom = txChanges.flatMap(_.toAppend)
    val (generator, withoutGenerator) = toAppendFrom.partition(_.proposition.address == gen.address)
    val generatorBox: PublicKey25519NoncedBox = (generator ++ boxesOf(gen)).headOption match {
      case Some(oldBox) =>
        oldBox.copy(nonce = oldBox.nonce + 1, value = oldBox.value + generatorReward)
      case None =>
        PublicKey25519NoncedBox(gen, 1, generatorReward)
    }
    val toAppend = (withoutGenerator ++ Seq(generatorBox)).map(b =>
      Insertion[Long, PublicKey25519Proposition, PublicKey25519NoncedBox](b))
    assert(toAppend.forall(_.box.value >= 0))

    StateChanges[Long, PublicKey25519Proposition, PublicKey25519NoncedBox](toRemove ++ toAppend)
  }
}

object SimpleState {
  val EmptyVersion: Array[Byte] = Array.fill(32)(0: Byte)
}