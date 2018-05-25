package scorex.testkit

import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.testkit.properties.mempool.MempoolFilterPerformanceTest

/**
  * Performance test for implementations
  */
trait BlockchainPerformance[
TX <: Transaction,
PM <: PersistentNodeViewModifier,
SI <: SyncInfo,
MPool <: MemoryPool[TX, MPool],
ST <: MinimalState[PM, ST],
HT <: History[PM, SI, HT]] extends MempoolFilterPerformanceTest[TX, MPool]
