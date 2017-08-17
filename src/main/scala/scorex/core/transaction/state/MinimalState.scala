package scorex.core.transaction.state

import scorex.core.transaction._
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.{NodeViewComponent, NodeViewModifier, PersistentNodeViewModifier}

import scala.util.Try

trait StateFeature

trait TransactionValidation[TX <: Transaction[_]] extends StateFeature {
  def isValid(tx: TX): Boolean = validate(tx).isSuccess

  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def validate(tx: TX): Try[Unit]
}


/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */

trait MinimalState[P <: Proposition,
BX <: Box[P],
TX <: Transaction[P],
M <: PersistentNodeViewModifier[P, TX],
MS <: MinimalState[P, BX, TX, M, MS]] extends NodeViewComponent {
  self: MS =>

  def version: VersionTag

  def validate(mod: M): Try[Unit]

  def applyModifier(mod: M): Try[MS]

  def applyModifiers(mods: Seq[M]): Try[MS] =
    mods.foldLeft(Try(this)) { case (curTry, mod) =>
      curTry flatMap (_.applyModifier(mod))
    }

  def rollbackTo(version: VersionTag): Try[MS]
}

object MinimalState {
  type VersionTag = NodeViewModifier.ModifierId
}