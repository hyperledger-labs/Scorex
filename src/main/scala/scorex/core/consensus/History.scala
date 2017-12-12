package scorex.core.consensus

import scorex.core._
import scorex.core.consensus.History.ProgressInfo
import scorex.crypto.encode.Base58

import scala.util.Try

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

trait History[PM <: PersistentNodeViewModifier, SI <: SyncInfo, HT <: History[PM, SI, HT]] extends HistoryReader[PM, SI] {

  /**
    * @return append modifier to history
    */
  def append(modifier: PM): Try[(HT, ProgressInfo[PM])]

  /**
    * Report that modifier is valid from other nodeViewHolder components point of view
    */
  def reportSemanticValidity(modifier: PM, valid: Boolean, lastApplied: ModifierId): (HT, ProgressInfo[PM])

  /**
    * @return read-only copy of this history
    */
  def getReader: HistoryReader[PM, SI] = this

}

object History {

  type ModifierIds = Seq[(ModifierTypeId, ModifierId)]

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)]): String = (ids.headOption, ids.lastOption) match {
    case (Some(f), Some(l)) if f._2 sameElements l._2 => s"[(${f._1},${Base58.encode(f._2)})]"
    case (Some(f), Some(l)) => s"[(${f._1},${Base58.encode(f._2)})..(${l._1},${Base58.encode(l._2)})]"
    case _ => "[]"
  }

  object HistoryComparisonResult extends Enumeration {
    val Equal = Value(1)
    val Younger = Value(2)
    val Older = Value(3)
    val Nonsense = Value(4)
    val Unknown = Value(5)
  }

  /**
    * Info returned by history to nodeViewHolder after modifier application
    *
    * @param branchPoint - branch point in case of rollback
    * @param toRemove    - modifiers to remove from current node view
    * @param toApply     - modifier to apply to current node view. Apply at most 1 modifier
    * @param toDownload  - modifiers to download from other nodes
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