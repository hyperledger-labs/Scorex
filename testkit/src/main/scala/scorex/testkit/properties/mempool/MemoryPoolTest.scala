package scorex.testkit.properties.mempool

import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.{MemoryPool, Transaction}


trait MemoryPoolTest[TX <: Transaction, MPool <: MemoryPool[TX, MPool]] {
  val memPool: MPool
  val memPoolGenerator: Gen[MPool]
  val transactionGenerator: Gen[TX]
}
