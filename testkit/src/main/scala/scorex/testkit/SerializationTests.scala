package scorex.testkit

import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.core.serialization.ScorexSerializer

trait SerializationTests extends ScalaCheckPropertyChecks with Matchers {
  def checkSerializationRoundtrip[A](generator: Gen[A], serializer: ScorexSerializer[A]): Assertion = {
    forAll(generator) { b: A =>
      val recovered = serializer.parseBytes(serializer.toBytes(b))
      serializer.toBytes(b) shouldEqual serializer.toBytes(recovered)
    }
  }
}
