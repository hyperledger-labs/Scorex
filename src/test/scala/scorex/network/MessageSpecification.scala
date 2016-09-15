package scorex.network

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.ObjectGenerators
import scorex.core.network.message.BasicMsgDataTypes.InvData
import scorex.core.network.message.{RequestModifierSpec, InvSpec}
import scorex.core.transaction.NodeViewModifier._

import scala.util.Try

class MessageSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers
with ObjectGenerators {

  property("InvData should remain the same after serialization/deserialization") {
    forAll(invDataGen) { data: InvData =>
      whenever(data._2.length < InvSpec.MaxObjects) {
        val bytes = InvSpec.serializeData(data)
        val recovered = InvSpec.deserializeData(bytes).get
        val bytes2 = InvSpec.serializeData(recovered)
        bytes shouldEqual bytes2
      }
    }
  }

  property("InvData should not serialize big arrays") {
    forAll(Arbitrary.arbitrary[Byte], Gen.listOfN(InvSpec.MaxObjects + 1, modifierIdGen)) { (b: Byte, s: Seq[ModifierId]) =>
      val data: InvData = (b, s)
      Try(InvSpec.serializeData(data)).isSuccess shouldBe false
    }
  }

  property("RequestModifierSpec serialization/deserialization") {
    forAll(invDataGen) { data: InvData =>
      whenever(data._2.length < InvSpec.MaxObjects) {
        val bytes = RequestModifierSpec.serializeData(data)
        val recovered = InvSpec.deserializeData(bytes).get
        val bytes2 = RequestModifierSpec.serializeData(recovered)
        bytes shouldEqual bytes2
      }
    }
  }

  property("RequestModifierSpec should not serialize big arrays") {
    forAll(Arbitrary.arbitrary[Byte], Gen.listOfN(InvSpec.MaxObjects + 1, modifierIdGen)) { (b: Byte, s: Seq[ModifierId]) =>
      val data: InvData = (b, s)
      Try(InvSpec.serializeData(data)).isSuccess shouldBe false
    }
  }


}
