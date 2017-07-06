package scorex.testkit.properties

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.{MemoryPool, Transaction}

trait MempoolTransactionsTest[P <: Proposition,
TX <: Transaction[P],
MPool <: MemoryPool[TX, MPool],
] extends PropSpec
  with GeneratorDrivenPropertyChecks with Matchers with PropertyChecks {

  val mempool: MPool
  val transactionGenerator: Gen[TX]
  //val history: HT
  //def genValidModifier(history: HT, mempoolTransactionFetchOption: Boolean): PM
  //var j = 0

  property("Transactions added to memory pool should be available by id") {
    //println(j)
    //j += 1
    var m: MPool = mempool
    forAll(transactionGenerator) { tx: TX =>
      m = m.put(tx).get
      //println(tx)
      //j += 1
      m.getById(tx.id).isDefined shouldBe true
    }
    //println(Seq[m.take(2)])
    /*var i=0
    var j = m.take(4)
    forAll(j){
      tx => println(tx.size)
        //println(i)
        i = i+1
    }*/
    // transactions have been added to the pool
    /*println("head : "+m.take(1).head)
    println(m.take(2).toVector(0))
    println(m.take(2).toVector(1))*/
    //println("mempool size before block formation is : " + mempool.size)
    //val block = genValidModifier(history, true)
    //println("new block transactions size : "+block.transactions.get.size)
    //println("new block transactions : "+block.transactions)
    //println("mempool size now is : " + mempool.size)
  }


}
