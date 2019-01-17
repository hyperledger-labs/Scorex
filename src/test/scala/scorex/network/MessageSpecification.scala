package scorex.network

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.ObjectGenerators
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.network.message.{InvSpec, ModifiersSpec, RequestModifierSpec}
import scorex.core.ModifierTypeId
import scorex.util.ModifierId

import scala.util.Try

class MessageSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ObjectGenerators {

  private val maxInvObjects = 500

  property("InvData should remain the same after serialization/deserialization") {
    val invSpec = new InvSpec(maxInvObjects)
    forAll(invDataGen) { data: InvData =>
      whenever(data._2.length < maxInvObjects) {
        val byteString = invSpec.toByteString(data)
        val recovered = invSpec.parseByteString(byteString)
        val byteString2 = invSpec.toByteString(recovered)
        byteString shouldEqual byteString2
      }
    }
  }

  property("InvData should not serialize big arrays") {
    val invSpec = new InvSpec(maxInvObjects)
    forAll(modifierTypeIdGen, Gen.listOfN(maxInvObjects + 1, modifierIdGen)) { (b: ModifierTypeId, s: Seq[ModifierId]) =>
      val data: InvData = (b, s)
      Try(invSpec.toByteString(data)).isSuccess shouldBe false
    }
  }

  property("RequestModifierSpec serialization/deserialization") {
    val requestModifierSpec = new RequestModifierSpec(maxInvObjects)
    forAll(invDataGen) { case data =>
      whenever(data._2.length < maxInvObjects) {
        val byteString = requestModifierSpec.toByteString(data)
        val recovered = requestModifierSpec.parseByteString(byteString)
        recovered._2.length shouldEqual data._2.length
        val byteString2 = requestModifierSpec.toByteString(recovered)
        byteString shouldEqual byteString2
      }
    }
  }

  property("RequestModifierSpec should not serialize big arrays") {
    val invSpec = new InvSpec(maxInvObjects)
    forAll(modifierTypeIdGen, Gen.listOfN(maxInvObjects + 1, modifierIdGen)) { (b: ModifierTypeId, s: Seq[ModifierId]) =>
      val data: InvData = (b, s)
      Try(invSpec.toByteString(data)).isSuccess shouldBe false
    }
  }

  property("ModifiersSpec serialization/deserialization") {
    forAll(modifiersGen) { data: (ModifierTypeId, Map[ModifierId, Array[Byte]]) =>
      whenever(data._2.nonEmpty) {
        val modifiersSpec = new ModifiersSpec(1024 * 1024)

        val bytes = modifiersSpec.toByteString(data)
        val recovered = modifiersSpec.parseByteString(bytes)

        recovered._1 shouldEqual data._1
        recovered._2.keys.size shouldEqual data._2.keys.size

        recovered._2.keys.foreach { id =>
          data._2.get(id).isDefined shouldEqual true
        }

        recovered._2.values.toSet.foreach { v: Array[Byte] =>
          data._2.values.toSet.exists(_.sameElements(v)) shouldEqual true
        }

        modifiersSpec.toByteString(data) shouldEqual bytes

        val modifiersSpecLimited = new ModifiersSpec(6)
        val bytes2 = modifiersSpecLimited.toByteString(data)
        val recovered2 = modifiersSpecLimited.parseByteString(bytes2)

        recovered2._1 shouldEqual data._1
        (recovered2._2.keys.size == data._2.keys.size) shouldEqual false
        recovered2._2.keys.size shouldEqual 0
      }
    }
  }
}
