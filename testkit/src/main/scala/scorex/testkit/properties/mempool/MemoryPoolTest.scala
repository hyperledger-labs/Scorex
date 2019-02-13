package scorex.testkit.properties.mempool

import akka.actor.{ActorRef, ActorSystem}
import org.scalacheck.Gen
import org.scalatest.{BeforeAndAfterAll, Suite}
import scorex.core.transaction.Transaction


trait MemoryPoolTest[TX <: Transaction] extends BeforeAndAfterAll { this: Suite =>

  type MPool = MempoolFixture[TX]

  protected def createMempoolActor(system: ActorSystem): ActorRef

  val transactionGenerator: Gen[TX]

  @volatile private var defaultMempoolAccessed: Boolean = false

  lazy val defaultMempool: MPool = {
    defaultMempoolAccessed = true
    new MempoolFixture(createMempoolActor)
  }

  override protected def afterAll(): Unit = {
    if (defaultMempoolAccessed) defaultMempool.close()
    super.afterAll()
  }

  /** Mempool generator that clean mempool on every generation and reuse it
    */
  def mempoolGenerator: Gen[MPool] = defaultMempool.mempoolGenerator

  def withNewMempool(block: MPool => Any): Unit = {
    val fixture = new MempoolFixture[TX](createMempoolActor)
    try {
      block(fixture)
    } finally {
      fixture.close()
    }
  }
}

