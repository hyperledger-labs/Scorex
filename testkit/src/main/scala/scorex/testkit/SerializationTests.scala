package scorex.testkit

import org.scalacheck.Gen
import org.scalatest.{Assertion, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scorex.core.newserialization.ScorexSerializer

trait SerializationTests extends GeneratorDrivenPropertyChecks with Matchers {
  def checkSerializationRoundtrip[A](generator: Gen[A], serializer: ScorexSerializer[A]): Assertion = {
    forAll(generator) { b: A =>
      val recovered = serializer.parseBytes(serializer.toBytes(b))
      serializer.toBytes(b) shouldEqual serializer.toBytes(recovered)
    }
  }
}
