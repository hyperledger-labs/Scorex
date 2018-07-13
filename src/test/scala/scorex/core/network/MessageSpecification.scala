package scorex.core.network

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.ObjectGenerators
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.network.message.{InvSpec, ModifiersSpec, RequestModifierSpec}
import scorex.core.{ModifierId, ModifierTypeId}

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
      whenever(data._2.length < maxInvObjects && data._2.nonEmpty) {
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
      val bytes = ModifiersSpec.toBytes(data)
      val recovered = ModifiersSpec.parseBytes(bytes).get

      recovered._1 shouldEqual data._1
      recovered._2.keys.size shouldEqual data._2.keys.size

      recovered._2.keys.foreach { id =>
        data._2.contains(id) shouldEqual true
      }

      recovered._2.values.toSet.foreach { v: Array[Byte] =>
        data._2.values.toSet.contains(v) shouldEqual true
      }

      ModifiersSpec.toBytes(data) shouldEqual bytes
    }
  }
}
