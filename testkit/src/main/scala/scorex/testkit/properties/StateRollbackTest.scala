package scorex.testkit.properties

import org.scalacheck.Gen
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.{BoxTransaction, MemoryPool}
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state._
import scorex.mid.state.BoxMinimalState
import scorex.testkit.TestkitHelpers


trait StateRollbackTest[P <: Proposition,
TX <: BoxTransaction[P, B],
PM <: PersistentNodeViewModifier[P, TX],
B <: Box[P],
ST <: BoxMinimalState[P, B, TX, PM, ST],
SI <: SyncInfo,
HT <: History[P, TX, PM, SI, HT],
MPool <: MemoryPool[TX, MPool]] extends StateTests[P, TX, PM, B, ST] with TestkitHelpers {

  val stateChangesGenerator: Gen[BoxStateChanges[P, B]]
  val history: HT
  val mempool: MPool
  val transactionGenerator: Gen[TX]

  def genValidModifier(history: HT, mempoolTransactionFetchOption: Boolean, noOfTransactionsFromMempool: Int): PM

  def genValidTransactionPair(curHistory: HT): Seq[TX]

  def genValidModifierCustomTransactions(curHistory: HT, trx: TX): PM

  property("State version updates as expected") {

    var lastVersion = state.version
    var newState = state

    forAll(stateChangesGenerator, modifierIdGen) { (c, newVersion) =>
      lastVersion shouldEqual newState.version

      newState = newState.applyChanges(c, newVersion).get
      newState.version shouldBe newVersion
      lastVersion = newVersion
    }
  }

  property("State changes application and rollback leads to the same state") {

    var newState = state
    var rollbackVersionOpt: Option[Array[Byte]] = None

    forAll(stateChangesGenerator, modifierIdGen) { (c, newVersion) =>
      rollbackVersionOpt match {
        case None =>
          newState = newState.applyChanges(c, newVersion).get
          rollbackVersionOpt = Some(newState.version)
        case Some(rollbackVersion) =>

          rollbackVersion shouldEqual newState.version

          newState = newState.applyChanges(c, newVersion).get
          newState.version shouldBe newVersion

          newState = newState.rollbackTo(rollbackVersion).get
          newState.version shouldEqual rollbackVersion
      }
    }
  }

  property("State changes application and rollback leads to rollback of changes") {
    var newState = state
    var rollbackVersionOpt: Option[Array[Byte]] = None
    var txPair = genValidTransactionPair(history)

    check { _ =>
      rollbackVersionOpt match {
        case None =>
          val randomTx = txPair.head
          val block = genValidModifierCustomTransactions(history, randomTx)
          val blockChanges = newState.changes(block).get

          val changes: BoxStateChanges[P, B] = BoxStateChanges(blockChanges.operations.flatMap { op =>
            op match {
              case rm: Removal[P, B] if newState.closedBox(rm.boxId).isEmpty => None
              case _ => Some(op)
            }
          })
          newState = newState.applyChanges(changes, block.id).get
          rollbackVersionOpt = Some(newState.version)

        case Some(rollbackVersion) =>
          val randomTx = txPair(1)
          val block = genValidModifierCustomTransactions(history, randomTx)
          val blockChanges = newState.changes(block).get

          val changes: BoxStateChanges[P, B] = BoxStateChanges(blockChanges.operations.flatMap { op =>
            op match {
              case rm: Removal[P, B] if newState.closedBox(rm.boxId).isEmpty => None
              case _ => Some(op)
            }
          })
          newState = newState.applyChanges(changes, block.id).get

          changes.toAppend.foreach { b =>
            newState.closedBox(b.box.id).isDefined shouldBe true
          }
          changes.toRemove.foreach { r =>
            newState.closedBox(r.boxId).isDefined shouldBe false
          }

          newState = newState.rollbackTo(rollbackVersion).get

          changes.toAppend.foreach { b =>
            newState.closedBox(b.box.id).isDefined shouldBe false
          }
          changes.toRemove.foreach { r =>
            newState.closedBox(r.boxId).isDefined shouldBe true
          }
          // Since transactions are created in pair where the first transaction creates a box and the second transaction uses
          // this box, so once the pair has been utilized we need to use a new pair for the further iterations.
          rollbackVersionOpt = None
          txPair = genValidTransactionPair(history)
      }
    }
  }
}
