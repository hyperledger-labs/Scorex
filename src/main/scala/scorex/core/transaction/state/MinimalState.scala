package scorex.core.transaction.state

import scorex.core.transaction._
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.{NodeViewComponent, NodeViewModifier, PersistentNodeViewModifier}

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */

trait MinimalState[T, P <: Proposition,
BX <: Box[P, T],
TX <: Transaction[P],
M <: PersistentNodeViewModifier[P, TX],
MS <: MinimalState[T, P, BX, TX, M, MS]] extends NodeViewComponent {
  self: MS =>

  def version: VersionTag

  def validate(transaction: TX): Try[Unit]

  def validate(mod: M): Try[Unit] = Try(mod.transactions.getOrElse(Seq()).foreach(tx => validate(tx).get))

  def isValid(tx: TX): Boolean = validate(tx).isSuccess

  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def closedBox(boxId: Array[Byte]): Option[BX]

  def boxesOf(proposition: P): Seq[BX]

  def changes(mod: M): Try[StateChanges[T, P, BX]]

  def applyChanges(changes: StateChanges[T, P, BX], newVersion: VersionTag): Try[MS]

  def applyModifier(mod: M): Try[MS] = {
    validate(mod) flatMap {_ =>
      changes(mod).flatMap(cs => applyChanges(cs, mod.id))
    }
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