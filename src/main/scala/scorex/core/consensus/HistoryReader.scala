package scorex.core.consensus

import scorex.core._


trait HistoryReader[PM <: PersistentNodeViewModifier, SI <: SyncInfo] extends NodeViewComponent {

  import History._

  /**
    * Is there's no history, even genesis block
    */
  def isEmpty: Boolean

  /**
    * Whether the history contains the given modifier
    *
    * @param persistentModifier - modifier
    * @return
    */
  def contains(persistentModifier: PM): Boolean = contains(persistentModifier.id)

  /**
    * Whether the history contains a modifier with the given id
    *
    * @param id - modifier's id
    * @return
    */
  def contains(id: ModifierId): Boolean = modifierById(id).isDefined

  /**
    * Whether a modifier could be applied to the history
    *
    * @param modifier - modifier to apply
    * @return
    */
  def applicable(modifier: PM): Boolean = openSurfaceIds().exists(_ sameElements modifier.parentId)

  /**
    * Return modifier of type PM with id == modifierId
    *
    * @param modifierId - modifier id to get from history
    * @return
    */
  def modifierById(modifierId: ModifierId): Option[PM]

  /**
    * Return semantic validity status of modifier with id == modifierId
    *
    * @param modifierId - modifier id to check
    * @return
    */
  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity.Value

  //todo: output should be ID | Seq[ID]
  def openSurfaceIds(): Seq[ModifierId]

  /**
    * Ids of modifiers, that node with info should download and apply to synchronize
    */
  def continuationIds(info: SI, size: Int): Option[ModifierIds]

  /**
    * Information about our node synchronization status. Other node should be able to compare it's view with ours by
    * this syncInfo message and calculate modifiers missed by our node.
    *
    * @param answer
    * @return
    */
  def syncInfo(answer: Boolean): SI

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  def compare(other: SI): HistoryComparisonResult.Value
}