package scorex.testkit

import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.authenticated.BoxMinimalState
import scorex.core.transaction.{BoxTransaction, MemoryPool, Transaction}
import scorex.testkit.properties._

/**
  * The idea of this class is to get some generators and test some situations, common for all blockchains
  */
trait BlockchainSanity[P <: Proposition,
TX <: BoxTransaction[P, B],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo,
B <: Box[P],
MPool <: MemoryPool[TX, MPool],
ST <: BoxMinimalState[P, B, TX, PM, ST],
HT <: History[P, TX, PM, SI, HT]] extends HistoryAppendBlockTest[P, TX, PM, SI, HT]
  with StateApplyChangesTest[P, TX, PM, B, ST]
  with WalletSecretsTest[P, TX, PM]
  with StateRollbackTest[P, TX, PM, B, ST, SI, HT, MPool]
  with MempoolTransactionsTest[P, TX, MPool]
  with MempoolFilterPerformanceTest[P, TX, MPool]
  with MempoolRemovalTest[P, TX, MPool, PM, HT, SI]
  with BoxStateChangesGenerationTest[P, TX, PM, B, ST, SI, HT]
{


}
