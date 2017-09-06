package scorex.network

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.ObjectGenerators
import scorex.core.{ModifierId, ModifierTypeId, NodeViewModifier}
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.network.message.{InvSpec, ModifiersSpec, RequestModifierSpec}

import scala.util.Try

class MessageSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ObjectGenerators {

  property("InvData should remain the same after serialization/deserialization") {
    forAll(invDataGen) { data: InvData =>
      whenever(data._2.length < InvSpec.MaxObjects) {
        val bytes = InvSpec.toBytes(data)
        val recovered = InvSpec.parseBytes(bytes).get
        val bytes2 = InvSpec.toBytes(recovered)
        bytes shouldEqual bytes2
      }
    }
  }

  property("InvData should not serialize big arrays") {
    forAll(modifierTypeIdGen, Gen.listOfN(InvSpec.MaxObjects + 1, modifierIdGen)) { (b: ModifierTypeId, s: Seq[ModifierId]) =>
      val data: InvData = (b, s)
      Try(InvSpec.toBytes(data)).isSuccess shouldBe false
    }
  }

  property("RequestModifierSpec serialization/deserialization") {
    forAll(invDataGen) { data: InvData =>
      whenever(data._2.length < InvSpec.MaxObjects) {
        val bytes = RequestModifierSpec.toBytes(data)
        val recovered = RequestModifierSpec.parseBytes(bytes).get
        recovered._2.length shouldEqual data._2.length
        val bytes2 = RequestModifierSpec.toBytes(recovered)
        bytes shouldEqual bytes2
      }
    }
  }

  property("RequestModifierSpec should not serialize big arrays") {
    forAll(modifierTypeIdGen, Gen.listOfN(InvSpec.MaxObjects + 1, modifierIdGen)) { (b: ModifierTypeId, s: Seq[ModifierId]) =>
      val data: InvData = (b, s)
      Try(InvSpec.toBytes(data)).isSuccess shouldBe false
    }
  }

  property("ModifiersSpec serialization/deserialization") {
    forAll(modifiersGen) { data: (ModifierTypeId, Map[ModifierId, Array[Byte]]) =>
      whenever(data._2.nonEmpty && data._2.forall { case (id, m) => id.length == NodeViewModifier.ModifierIdSize && m.length > 0 }) {
        val bytes = ModifiersSpec.toBytes(data)
        val recovered = ModifiersSpec.parseBytes(bytes).get

        recovered._1 shouldEqual data._1
        recovered._2.keys.size shouldEqual data._2.keys.size

        recovered._2.keys.foreach { id =>
          data._2.keys.exists(_.sameElements(id)) shouldEqual true
        }

        recovered._2.values.toSet.foreach { v: Array[Byte] =>
          data._2.values.toSet.exists(_.sameElements(v)) shouldEqual true
        }

        ModifiersSpec.toBytes(data) shouldEqual bytes
      }
    }
  }
}
