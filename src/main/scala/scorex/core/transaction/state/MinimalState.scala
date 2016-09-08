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
trait MinimalState[P <: Proposition, TX <: Transaction[P, TX], M <: PersistentNodeViewModifier[P, TX]] extends NodeViewComponent {
  type VersionTag = NodeViewModifier.ModifierId

  def version: VersionTag

  def isValid(tx: TX): Boolean = tx.validate(this).isSuccess

  def areValid(txs: Seq[TX]): Boolean = txs.forall(isValid)

  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def closedBox(boxId: Array[Byte]): Option[Box[P]]

  def accountBox(p: P): Option[Box[P]]

  def applyChanges(change: StateChanges[P]): Try[MinimalState[P, TX, M]]

  def applyChanges(mod: M): Try[MinimalState[P, TX, M]]

  def rollbackTo(version: VersionTag): Try[MinimalState[P, TX, M]]
}
