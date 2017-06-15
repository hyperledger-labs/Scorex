package scorex.testkit

import org.scalacheck.Gen
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scorex.core.serialization.{BytesSerializable, Serializer}

trait SerializationTests extends GeneratorDrivenPropertyChecks with Matchers {

  def checkSerializationRoundtrip[A <: BytesSerializable](generator: Gen[A]): Unit = {
    forAll(generator) { b: A =>
      val serializer: Serializer[A] = b.serializer.asInstanceOf[Serializer[A]]
      val recovered = serializer.parseBytes(serializer.toBytes(b)).get
      serializer.toBytes(b) shouldEqual serializer.toBytes(recovered)
    }
  }

}
