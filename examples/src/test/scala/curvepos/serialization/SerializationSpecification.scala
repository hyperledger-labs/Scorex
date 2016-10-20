package curvepos.serialization


import curvepos.ExampleGenerators
import examples.curvepos.serialization.CurveposRegistrar
import examples.curvepos.transaction.{SimpleBlock, SimplePayment}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.core.serialization.{ScorexKryoPool, ScorexRegistrar}

class SerializationSpecification extends PropSpec with GeneratorDrivenPropertyChecks with Matchers
with ExampleGenerators {

  private val pool = new ScorexKryoPool(new ScorexRegistrar, new CurveposRegistrar)

  property("SimplePayment serialization roundtrip") {
    checkSerialization[SimplePayment](paymentGen, classOf[SimplePayment])
  }

  property("SimpleBlock serialization roundtrip") {
    checkSerialization[SimpleBlock](blockGen, classOf[SimpleBlock])
  }

  def checkSerialization[T](gen: Gen[T], cls: Class[T], check: (T, T) => Unit = (o1: T, o2: T) => o1 shouldBe o2) = {
    forAll(gen) { obj: T =>
      val bytes = pool.toBytesWithoutClass(obj)
      val obj2 = pool.fromBytes(bytes, cls).get
      check(obj2, obj)
      val bytes2 = pool.toBytesWithoutClass(obj2)
      bytes shouldEqual bytes2
    }
  }
}
