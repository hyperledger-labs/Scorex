package scorex.network

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.ObjectGenerators
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.network.message.{InvSpec, ModifiersSpec, RequestModifierSpec}
import scorex.core.ModifierTypeId
import scorex.core.app.Version
import scorex.util.ModifierId

import scala.util.Try

class MessageSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ObjectGenerators {

  private val maxInvObjects = 500

  property("version comparison") {
    Version(10, 10, 10) > Version(10, 10, 9) shouldBe true
    Version(10, 10, 10) > Version(10, 9, 11) shouldBe true
    Version(10, 10, 10) > Version(9, 11, 11) shouldBe true
    Version(10, 10, 10) == Version(10, 10, 10) shouldBe true
  }

  property("InvData should remain the same after serialization/deserialization") {
    val invSpec = new InvSpec(maxInvObjects)
    forAll(invDataGen) { data: InvData =>
      whenever(data._2.length < maxInvObjects) {
        val bytes = invSpec.toBytes(data)
        val recovered = invSpec.parseBytes(bytes).get
        val bytes2 = invSpec.toBytes(recovered)
        bytes shouldEqual bytes2
      }
    }
  }

  property("InvData should not serialize big arrays") {
    val invSpec = new InvSpec(maxInvObjects)
    forAll(modifierTypeIdGen, Gen.listOfN(maxInvObjects + 1, modifierIdGen)) { (b: ModifierTypeId, s: Seq[ModifierId]) =>
      val data: InvData = (b, s)
      Try(invSpec.toBytes(data)).isSuccess shouldBe false
    }
  }

  property("RequestModifierSpec serialization/deserialization") {
    val requestModifierSpec = new RequestModifierSpec(maxInvObjects)
    forAll(invDataGen) { data: InvData =>
      whenever(data._2.length < maxInvObjects) {
        val bytes = requestModifierSpec.toBytes(data)
        val recovered = requestModifierSpec.parseBytes(bytes).get
        recovered._2.length shouldEqual data._2.length
        val bytes2 = requestModifierSpec.toBytes(recovered)
        bytes shouldEqual bytes2
      }
    }
  }

  property("RequestModifierSpec should not serialize big arrays") {
    val invSpec = new InvSpec(maxInvObjects)
    forAll(modifierTypeIdGen, Gen.listOfN(maxInvObjects + 1, modifierIdGen)) { (b: ModifierTypeId, s: Seq[ModifierId]) =>
      val data: InvData = (b, s)
      Try(invSpec.toBytes(data)).isSuccess shouldBe false
    }
  }

  property("ModifiersSpec serialization/deserialization") {
    forAll(modifiersGen) { data: (ModifierTypeId, Map[ModifierId, Array[Byte]]) =>
      whenever(data._2.nonEmpty) {
        val modifiersSpec = new ModifiersSpec(1024 * 1024)

        val bytes = modifiersSpec.toBytes(data)
        val recovered = modifiersSpec.parseBytes(bytes).get

        recovered._1 shouldEqual data._1
        recovered._2.keys.size shouldEqual data._2.keys.size

        recovered._2.keys.foreach { id =>
          data._2.get(id).isDefined shouldEqual true
        }

        recovered._2.values.toSet.foreach { v: Array[Byte] =>
          data._2.values.toSet.exists(_.sameElements(v)) shouldEqual true
        }

        modifiersSpec.toBytes(data) shouldEqual bytes

        val modifiersSpecLimited = new ModifiersSpec(6)
        val bytes2 = modifiersSpecLimited.toBytes(data)
        val recovered2 = modifiersSpecLimited.parseBytes(bytes2).get

        recovered2._1 shouldEqual data._1
        (recovered2._2.keys.size == data._2.keys.size) shouldEqual false
        recovered2._2.keys.size shouldEqual 0
      }
    }
  }
}
