package io.scalac.elm.transaction

import java.nio.ByteBuffer

import io.scalac.elm.transaction.ElmMinState._
import scorex.core.NodeViewComponentCompanion
import scorex.core.block.StateChanges
import scorex.core.transaction.TransactionChanges
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.{Failure, Try}


object ElmMinState {
  val EmptyVersion: Array[Byte] = Array.fill(32)(0: Byte)
}

case class ElmMinState(version: VersionTag = EmptyVersion, storage: Map[ByteBuffer, TxOutput] = Map())
  extends ScorexLogging
  with MinimalState[PublicKey25519Proposition, TxOutput, ElmTransaction, ElmBlock, ElmMinState] {

  def isEmpty: Boolean = version sameElements EmptyVersion

  def totalBalance: Long = storage.keySet.flatMap(k => storage.get(k).map(_.value)).sum

  override def toString: String = {
    s"SimpleState at ${Base58.encode(version)}\n" + storage.keySet.flatMap(k => storage.get(k)).mkString("\n  ")
  }

  override def boxOf(p: PublicKey25519Proposition): Seq[TxOutput] =
    storage.values.filter(b => b.proposition.address == p.address).toSeq

  override def closedBox(boxId: Array[Byte]): Option[TxOutput] =
    storage.get(ByteBuffer.wrap(boxId))

  override def rollbackTo(version: VersionTag): Try[ElmMinState] = {
    log.warn("Rollback is not implemented")
    Try(this)
  }

  override def applyChanges(change: StateChanges[PublicKey25519Proposition, TxOutput], newVersion: VersionTag): Try[ElmMinState] = Try {
    val rmap = change.toRemove.foldLeft(storage) { case (m, b) => m - ByteBuffer.wrap(b.id) }

    val amap = change.toAppend.foldLeft(rmap) { case (m, b) =>
      require(b.value >= 0)
      m + (ByteBuffer.wrap(b.id) -> b)
    }
    ElmMinState(newVersion, amap)
  }

  override def companion: NodeViewComponentCompanion = ???

  override type NVCT = ElmMinState

  override def validate(transaction: ElmTransaction): Try[Unit] = transaction match {
//    case sp: SimplePayment => Try {
//      val b = boxOf(sp.sender).head
//      (b.value >= Math.addExact(sp.amount, sp.fee)) && (b.nonce + 1 == sp.nonce)
//    }
    case _ => Try(())
  }

  /**
    * A Transaction opens existing boxes and creates new ones
    */
  override def changes(transaction: ElmTransaction): Try[TransactionChanges[PublicKey25519Proposition, TxOutput]] =  ??? /*{
    transaction match {
      case tx: SimplePayment if !isEmpty => Try {
        val oldSenderBox = boxOf(tx.sender).head
        val oldRecipientBox = boxOf(tx.recipient).headOption
        val newRecipientBox = oldRecipientBox.map { oldB =>
          oldB.copy(nonce = oldB.nonce + 1, value = Math.addExact(oldB.value, tx.amount))
        }.getOrElse(PublicKey25519NoncedBox(tx.recipient, tx.amount))
        val newSenderBox = oldSenderBox.copy(nonce = oldSenderBox.nonce + 1,
          value = Math.addExact(Math.addExact(oldSenderBox.value, -tx.amount), -tx.fee))
        val toRemove = Set(oldSenderBox) ++ oldRecipientBox
        val toAppend = Set(newRecipientBox, newSenderBox).ensuring(_.forall(_.value >= 0))

        TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](toRemove, toAppend, tx.fee)
      }
      case genesis: SimplePayment if isEmpty => Try {
        val toAppend: Set[PublicKey25519NoncedBox] = Set(PublicKey25519NoncedBox(genesis.recipient, genesis.amount))
        TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](Set(), toAppend, 0)
      }
      case _ => Failure(new Exception("implementation is needed"))
    }
  }*/

  override def changes(block: ElmBlock): Try[StateChanges[PublicKey25519Proposition, TxOutput]] = ??? /*Try {
    val generatorReward = block.txs.map(_.fee).sum
    val gen = block.generator

    val txChanges = block.txs.map(tx => changes(tx)).map(_.get)
    val toRemove: Set[PublicKey25519NoncedBox] = txChanges.flatMap(_.toRemove).toSet
    val toAppendFrom: Set[PublicKey25519NoncedBox] = txChanges.flatMap(_.toAppend).toSet
    val (generator, withoutGenerator) = toAppendFrom.partition(_.proposition.address == gen.address)
    val generatorBox: PublicKey25519NoncedBox = (generator ++ boxOf(gen)).headOption match {
      case Some(oldBox) =>
        oldBox.copy(nonce = oldBox.nonce + 1, value = oldBox.value + generatorReward)
      case None =>
        PublicKey25519NoncedBox(gen, 1, generatorReward)
    }
    val toAppend = withoutGenerator + generatorBox
    require(toAppend.forall(_.value >= 0))

    StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](toRemove, toAppend)
  }*/
}