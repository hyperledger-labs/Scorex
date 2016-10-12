package scorex.core.transaction.state

import scorex.core.{NodeViewComponent, NodeViewModifier, PersistentNodeViewModifier}
import scorex.core.block.StateChanges
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction._

import scala.util.Try
import MinimalState.VersionTag

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */

trait MinimalState[P <: Proposition,
BX <: Box[P],
TX <: Transaction[P],
M <: PersistentNodeViewModifier[P, TX], MS <: MinimalState[P, BX, TX, M, MS]] extends NodeViewComponent {
  self: MS =>

  def version: VersionTag

  def isValid(tx: TX): Boolean = validate(tx).isSuccess

  def areValid(txs: Seq[TX]): Boolean = txs.forall(isValid)

  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def closedBox(boxId: Array[Byte]): Option[BX]

  def validate(transaction: TX): Try[Unit]

  def boxOf(proposition: P): Seq[BX]

  /**
    * A Transaction opens existing boxes and creates new ones
    */
  def changes(transaction: TX): Try[TransactionChanges[P, BX]]

  def changes(mod: M): Try[StateChanges[P, BX]]

  def applyChanges(changes: StateChanges[P, BX], newVersion: VersionTag): Try[MS]

  def applyModifier(mod: M): Try[MS] = {
    val newVersion = mod.id
    changes(mod).flatMap(cs => applyChanges(cs, newVersion))
  }

  def applyModifiers(mods: Seq[M]): Try[MS] =
    mods.foldLeft(Try(this)) { case (curTry, mod) =>
      curTry flatMap (_.applyModifier(mod))
    }

  def rollbackTo(version: VersionTag): Try[MS]
}

object MinimalState {
  type VersionTag = NodeViewModifier.ModifierId
}