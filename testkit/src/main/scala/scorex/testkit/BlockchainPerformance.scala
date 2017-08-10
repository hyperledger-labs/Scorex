package scorex.testkit

import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.testkit.properties._
import scorex.testkit.properties.mempool.MempoolFilterPerformanceTest

/**
  * Performance test for implementations
  */
trait BlockchainPerformance[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo,
B <: Box[P],
MPool <: MemoryPool[TX, MPool],
ST <: MinimalState[P, B, TX, PM, ST],
HT <: History[P, TX, PM, SI, HT]] extends MempoolFilterPerformanceTest[P, TX, MPool]
