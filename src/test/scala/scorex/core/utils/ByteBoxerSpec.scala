package scorex.core.utils

import org.scalatest.{FlatSpec, Matchers}
import scorex.core.ModifierId
import supertagged.tag

class ByteBoxerSpec extends FlatSpec with Matchers {

  val bytesA: Array[Byte] = Array(1, 2, 3, 4)
  val bytesB: Array[Byte] = Array(11, 12, 13, 14)

  val boxerA = ByteBoxer[ModifierId](tag[ModifierId](bytesA))
  val boxerB = ByteBoxer[ModifierId](tag[ModifierId](bytesA))
  val boxerC = ByteBoxer[ModifierId](tag[ModifierId](bytesB))

  "hashCode" should "be the same if the underlying data is the same" in {
    boxerA.hashCode shouldBe boxerB.hashCode
  }

  "hashCode" should "not be the same if the underlying data is not the same" in {
    boxerA.hashCode shouldNot equal(boxerC.hashCode)
  }

  "equals" should "be true if the underlying data is the same " in {
    boxerA.equals(boxerB) shouldBe true
  }

  "equals" should "be false if the underlying data is not the same " in {
    boxerA.equals(boxerC) shouldBe false
  }

  "toString" should "produce a hex string" in {
    boxerA.toString shouldBe "ByteBoxer[01020304]"
  }
}
