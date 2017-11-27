package scorex.core.consensus

import scorex.core._
import scorex.crypto.encode.Base58

import scala.util.Try

object ModifierSemanticValidity extends Enumeration {
  val Absent = Value(0)
  val Unknown = Value(1)
  val Valid = Value(2)
  val Invalid = Value(3)
}

/**
  * History of a blockchain system is some blocktree in fact
  * (like this: http://image.slidesharecdn.com/sfbitcoindev-chepurnoy-2015-150322043044-conversion-gate01/95/proofofstake-its-improvements-san-francisco-bitcoin-devs-hackathon-12-638.jpg),
  * where longest chain is being considered as canonical one, containing right kind of history.
  *
  * In cryptocurrencies of today blocktree view is usually implicit, means code supports only linear history,
  * but other options are possible.
  *
  * To say "longest chain" is the canonical one is simplification, usually some kind of "cumulative difficulty"
  * function has been used instead.
  */

trait History[PM <: PersistentNodeViewModifier, SI <: SyncInfo, HT <: History[PM, SI, HT]] extends NodeViewComponent {

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

  def modifierById(modifierId: ModifierId): Option[PM]

  //TODO never used?
  def modifierById(modifierId: String): Option[PM] = Base58.decode(modifierId).toOption
    .flatMap(id => modifierById(ModifierId @@ id))

  def append(modifier: PM): Try[(HT, ProgressInfo[PM])]

  def reportSemanticValidity(modifier: PM, valid: Boolean, lastApplied: ModifierId): (HT, ProgressInfo[PM])

  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity.Value

  //todo: output should be ID | Seq[ID]
  def openSurfaceIds(): Seq[ModifierId]

  /**
    * Ids of modifiers, that node with info should download and apply to synchronize
    * todo: argument should be ID | Seq[ID] ?
    */
  def continuationIds(info: SI, size: Int): Option[ModifierIds]

  def syncInfo(answer: Boolean): SI

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  def compare(other: SI): HistoryComparisonResult.Value
}

object History {

  type ModifierIds = Seq[(ModifierTypeId, ModifierId)]

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)]): String = {
    s"${ids.headOption.map(_._2).map(Base58.encode).getOrElse("None")}.." +
      s"${ids.lastOption.map(_._2).map(Base58.encode).getOrElse("None")}"
  }

  object HistoryComparisonResult extends Enumeration {
    val Equal = Value(1)
    val Younger = Value(2)
    val Older = Value(3)
    val Nonsense = Value(4)
  }

  /**
    * Info returned by history to nodeViewHolder after modifier application
    *
    * @param branchPoint - branch point in case of rollback
    * @param toRemove - modifiers to remove from current node view
    * @param toApply - modifier to apply to current node view. Apply at most 1 modifier
    * @param toDownload - modifiers to download from other nodes
    * @tparam PM - type of used modifier
    */
  case class ProgressInfo[PM <: PersistentNodeViewModifier](branchPoint: Option[ModifierId],
                                                            toRemove: Seq[PM],
                                                            toApply: Option[PM],
                                                            toDownload: Seq[(ModifierTypeId, ModifierId)]
                                                           ) {

    require(branchPoint.isDefined == toRemove.nonEmpty, s"Branch point should be defined for non-empty toRemove," +
      s" ${branchPoint.isDefined} == ${toRemove.nonEmpty} given")

    lazy val chainSwitchingNeeded: Boolean = toRemove.nonEmpty

    override def toString: String = {
      s"ProgressInfo(BranchPoint: ${branchPoint.map(Base58.encode)}, " +
        s" to remove: ${toRemove.map(_.encodedId)}, to apply: ${toApply.map(_.encodedId)})"
    }
  }
}