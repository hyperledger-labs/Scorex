package scorex.testkit

import scorex.core.{PersistentNodeViewModifier, TransactionsCarryingPersistentNodeViewModifier}
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.{BoxTransaction, MemoryPool}
import scorex.mid.state.BoxMinimalState
import scorex.testkit.generators.AllModifierProducers
import scorex.testkit.properties._
import scorex.testkit.properties.mempool.{MempoolFilterPerformanceTest, MempoolRemovalTest, MempoolTransactionsTest}
import scorex.testkit.properties.state.StateApplicationTest
import scorex.testkit.properties.state.box.{BoxStateApplyChangesTest, BoxStateChangesGenerationTest, BoxStateRollbackTest}

/**
  * The idea of this class is to get some generators and test some situations, common for all blockchains
  */
trait BlockchainSanity[P <: Proposition,
TX <: BoxTransaction[P, B],
PM <: PersistentNodeViewModifier,
CTM <: PM with TransactionsCarryingPersistentNodeViewModifier[TX],
SI <: SyncInfo,
B <: Box[P],
MPool <: MemoryPool[TX, MPool],
ST <: BoxMinimalState[P, B, TX, PM, ST],
HT <: History[PM, SI, HT]]
  extends
    BoxStateChangesGenerationTest[P, TX, PM, B, ST]
    with StateApplicationTest[PM, ST]
    with HistoryTests[TX, PM, SI, HT]
    with BoxStateApplyChangesTest[P, TX, PM, B, ST]
    with WalletSecretsTest[P, TX, PM]
    with BoxStateRollbackTest[P, TX, PM, CTM, B, ST]
    with MempoolTransactionsTest[TX, MPool]
    with MempoolFilterPerformanceTest[TX, MPool]
    with MempoolRemovalTest[TX, MPool, PM, CTM, HT, SI]
    with AllModifierProducers[TX, MPool, PM, CTM, ST, SI, HT]
    with NodeViewHolderTests[TX, PM, ST, SI, HT, MPool]
    with NodeViewSynchronizerTests[TX, PM, ST, SI, HT, MPool] {
}
