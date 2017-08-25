package scorex.testkit

import org.scalacheck.Gen
import org.scalatest.{Assertion, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scorex.core.serialization.Serializer

trait SerializationTests extends GeneratorDrivenPropertyChecks with Matchers {
  def checkSerializationRoundtrip[A](generator: Gen[A], serializer: Serializer[A]): Assertion = {
    forAll(generator) { b: A =>
      val recovered = serializer.parseBytes(serializer.toBytes(b)).get
      serializer.toBytes(b) shouldEqual serializer.toBytes(recovered)
    }
  }
}
