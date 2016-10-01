package scorex.core.transaction.state


import scorex.core.NodeViewComponent
import scorex.core.block.StateChanges
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction._

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */
trait MinimalState[P <: Proposition,
BX <: Box[P],
TX <: Transaction[P],
M <: PersistentNodeViewModifier[P, TX], MS <: MinimalState[P, BX, TX, M, MS]] extends NodeViewComponent {
  self: MS =>

  type VersionTag = NodeViewModifier.ModifierId

  def version: VersionTag

  def isValid(tx: TX): Boolean = validate(tx).isSuccess

  def areValid(txs: Seq[TX]): Boolean = txs.forall(isValid)

  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def closedBox(boxId: Array[Byte]): Option[BX]

  def validate(transaction: TX): Try[Unit]

  /**
    * A Transaction opens existing boxes and creates new ones
    */
  def changes(transaction: TX): Try[TransactionChanges[P, BX]]

  def boxOf(proposition: P): Seq[BX]

  def applyChanges(change: StateChanges[P, BX]): Try[MS]

  def applyChanges(mod: M): Try[MS]

  def applyChanges(mods: Seq[M]): Try[MS] =
    mods.foldLeft(Try(this)){case (curTry, mod) =>
        curTry flatMap (_.applyChanges(mod))
    }

  def rollbackTo(version: VersionTag): Try[MS]
}
