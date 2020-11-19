package scorex.network

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.ObjectGenerators
import scorex.core.network.message._
import scorex.core.ModifierTypeId
import scorex.core.app.Version
import scorex.util.ModifierId

import scala.util.Try

class MessageSpecification extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with ObjectGenerators {

  private val maxInvObjects = 500

  property("version comparison") {
    Version(10, 10, 10) > Version(10, 10, 9) shouldBe true
    Version(10, 10, 10) > Version(10, 9, 11) shouldBe true
    Version(10, 10, 10) > Version(9, 11, 11) shouldBe true
    Version(10, 10, 10) == Version(10, 10, 10) shouldBe true
  }

  property("InvData should remain the same after serializatiHandshakeon/deserialization") {
    val invSpec = new InvSpec(maxInvObjects)
    forAll(invDataGen) { data: InvData =>
      whenever(data.ids.length < maxInvObjects) {
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
      val data: InvData = InvData(b, s)
      Try(invSpec.toByteString(data)).isSuccess shouldBe false
    }
  }

  property("RequestModifierSpec serialization/deserialization") {
    val requestModifierSpec = new RequestModifierSpec(maxInvObjects)
    forAll(invDataGen) { case data =>
      whenever(data.ids.length < maxInvObjects) {
        val byteString = requestModifierSpec.toByteString(data)
        val recovered = requestModifierSpec.parseByteString(byteString)
        recovered.ids.length shouldEqual data.ids.length
        val byteString2 = requestModifierSpec.toByteString(recovered)
        byteString shouldEqual byteString2
      }
    }
  }

  property("RequestModifierSpec should not serialize big arrays") {
    val invSpec = new InvSpec(maxInvObjects)
    forAll(modifierTypeIdGen, Gen.listOfN(maxInvObjects + 1, modifierIdGen)) { (b: ModifierTypeId, s: Seq[ModifierId]) =>
      val data: InvData = InvData(b, s)
      Try(invSpec.toByteString(data)).isSuccess shouldBe false
    }
  }

  property("ModifiersSpec serialization/deserialization") {
    forAll(modifiersGen) { data: ModifiersData =>
      whenever(data.modifiers.nonEmpty) {
        val modifiersSpec = new ModifiersSpec(1024 * 1024)

        val bytes = modifiersSpec.toByteString(data)
        val recovered = modifiersSpec.parseByteString(bytes)

        recovered.typeId shouldEqual data.typeId
        recovered.modifiers.keys.size shouldEqual data.modifiers.keys.size

        recovered.modifiers.keys.foreach { id =>
          data.modifiers.get(id).isDefined shouldEqual true
        }

        recovered.modifiers.values.toSet.foreach { v: Array[Byte] =>
          data.modifiers.values.toSet.exists(_.sameElements(v)) shouldEqual true
        }

        modifiersSpec.toByteString(data) shouldEqual bytes

        val modifiersSpecLimited = new ModifiersSpec(6)
        val bytes2 = modifiersSpecLimited.toByteString(data)
        val recovered2 = modifiersSpecLimited.parseByteString(bytes2)

        recovered2.typeId shouldEqual data.typeId
        (recovered2.modifiers.keys.size == data.modifiers.keys.size) shouldEqual false
        recovered2.modifiers.keys.size shouldEqual 0
      }
    }
  }
}
