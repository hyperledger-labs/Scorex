package curvepos.serialization


import curvepos.ExampleGenerators
import examples.curvepos.SimpleSyncInfo
import examples.curvepos.serialization.CurveposRegistrar
import examples.curvepos.transaction.{SimpleBlock, SimplePayment}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.core.serialization.{ScorexKryoPool, ScorexRegistrar}

import scala.reflect.ClassTag

class SerializationSpecification extends PropSpec with GeneratorDrivenPropertyChecks with Matchers
with ExampleGenerators {

  private val pool = new ScorexKryoPool(new ScorexRegistrar, new CurveposRegistrar)

  property("SimplePayment serialization roundtrip") {
    checkSerialization[SimplePayment](paymentGen)
  }

  property("SimpleBlock serialization roundtrip") {
    checkSerialization[SimpleBlock](blockGenerator)
  }

  property("SimpleSyncInfo serialization roundtrip") {
    checkSerialization[SimpleSyncInfo](simpleSyncInfoGenerator, emptyCheck[SimpleSyncInfo])
  }

  def emptyCheck[T](o1: T, o2: T): Unit = {}

  def checkSerialization[T: ClassTag](gen: Gen[T], check: (T, T) => Unit = (o1: T, o2: T) => o1 shouldBe o2) = {
    forAll(gen) { obj: T =>
      val bytes = pool.toBytesWithoutClass(obj)
      val obj2 = pool.fromBytes[T](bytes).get
      check(obj2, obj)
      val bytes2 = pool.toBytesWithoutClass(obj2)
      bytes shouldEqual bytes2
    }
  }
}
