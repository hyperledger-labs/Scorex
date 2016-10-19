package scorex.serialization.binary

import java.net.InetSocketAddress

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.ObjectGenerators
import scorex.core.network.Handshake
import scorex.core.serialization.ScorexKryoInstantiator

class SerializationSpecification extends PropSpec with GeneratorDrivenPropertyChecks with Matchers
  with ObjectGenerators {

  private val pool = ScorexKryoInstantiator.defaultPool

  property("ByteLengthUtf8StringSerializer test") {
    checkSerialization[String](Arbitrary.arbitrary[String].suchThat(_.length < 80), classOf[String])
  }
  property("Handshake serialization roundtrip") {
    checkSerialization[Handshake](handshakeGen, classOf[Handshake])
  }

  property("InetSocketAddress serialization roundtrip") {
    checkSerialization[InetSocketAddress](inetAddressGen, classOf[InetSocketAddress])
  }

  def checkSerialization[T](gen: Gen[T], cls: Class[T], check: (T, T) => Unit = (o1: T, o2: T) => o1 shouldBe o2) = {
    forAll(gen) { obj: T =>
      val bytes = pool.toBytesWithoutClass(obj)
      val obj2 = pool.fromBytes(bytes, cls)
      check(obj2, obj)
      val bytes2 = pool.toBytesWithoutClass(obj2)
      bytes shouldEqual bytes2
    }
  }
}
