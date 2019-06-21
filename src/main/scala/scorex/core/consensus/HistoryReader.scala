package scorex.core.consensus

import scorex.core.{NodeViewComponent, PersistentNodeViewModifier}
import scorex.util.ModifierId

import scala.util.Try


trait HistoryReader[PM <: PersistentNodeViewModifier, SI <: SyncInfo] extends NodeViewComponent
  with ContainsModifiers[PM] {

  import History._

  /**
    * Is there's no history, even genesis block
    */
  def isEmpty: Boolean

   /**
    * Whether a modifier could be applied to the history
    *
    * @param modifier  - modifier to apply
    * @return `Success` if modifier can be applied, `Failure(ModifierError)` if can not
    */
  def applicableTry(modifier: PM): Try[Unit]

  /**
    * Return semantic validity status of modifier with id == modifierId
    *
    * @param modifierId - modifier id to check
    * @return
    */
  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity

  //todo: output should be ID | Seq[ID]
  def openSurfaceIds(): Seq[ModifierId]

  /**
    * Ids of modifiers, that node with info should download and apply to synchronize
    */
  def continuationIds(info: SI, size: Int): ModifierIds

  /**
    * Information about our node synchronization status. Other node should be able to compare it's view with ours by
    * this syncInfo message and calculate modifiers missed by our node.
    *
    * @return
    */
  def syncInfo: SI

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  def compare(other: SI): HistoryComparisonResult
}
