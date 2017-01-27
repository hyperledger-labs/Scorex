package scorex.testkit

import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.SyncInfo
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.testkit.properties._

/**
  * The idea of this class is to get some generators and test some situations, common for all blockchains
  */
trait BlockchainSanity[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo,
B <: Box[P],
MPool <: MemoryPool[TX, MPool]] extends HistoryAppendBlockTest[P, TX, PM, SI]
  with StateApplyChangesTest[P, TX, PM, B]
  with WalletSecretsTest[P, TX, PM]
  with StateRollbackTest[P, TX, PM, B]
  with MempoolTransactionsTest[P, TX, MPool]
  with StateChangesGenerationTest[P, TX, PM,  B] {


}
