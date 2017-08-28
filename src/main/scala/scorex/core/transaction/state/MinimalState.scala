package scorex.core.transaction.state

import scorex.core.transaction._
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.{NodeViewComponent, NodeViewModifier, PersistentNodeViewModifier}

import scala.util.{Failure, Success, Try}

trait StateFeature

trait TransactionValidation[P <: Proposition, TX <: Transaction[P]] extends StateFeature {
  def isValid(tx: TX): Boolean = validate(tx).isSuccess

  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def validate(tx: TX): Try[Unit]
}

trait ModifierValidation[M <: PersistentNodeViewModifier] extends StateFeature {
  def validate(mod: M): Try[Unit]
}

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */

trait MinimalState[M <: PersistentNodeViewModifier, MS <: MinimalState[M, MS]] extends NodeViewComponent {
  self: MS =>

  def version: VersionTag

  def applyModifier(mod: M): Try[MS]

  //todo: remove
  def applyModifiers(mods: Seq[M]): Try[MS] =
    mods.foldLeft(Try(this)) { case (curTry, mod) =>
      curTry flatMap (_.applyModifier(mod))
    } match {
      case s: Success[MS] => s
      case f: Failure[MS] => rollbackTo(version); f
    }

  def rollbackTo(version: VersionTag): Try[MS]
}

object MinimalState {
  type VersionTag = NodeViewModifier.ModifierId
}