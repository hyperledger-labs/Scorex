package curvepos

import examples.curvepos.{BaseTarget, GenerationSignature}
import examples.curvepos.transaction.{SimpleBlock, SimplePayment, SimpleState, SimpleTransaction}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.ModifierId

import scala.util.{Failure, Success, Try}

class SimpleStateSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ExampleGenerators {

  property("Total balance") {
    var state = new SimpleState() {
      override def validate(transaction: SimpleTransaction): Try[Unit] = Success()
    }
    state.totalBalance shouldBe 0
    state.isEmpty shouldBe true

    state = state.applyModifier(genesisBlock).get
    val GenesisBalance = state.totalBalance
    state.isEmpty shouldBe false
    GenesisBalance shouldBe Long.MaxValue
    val Mod = GenesisBalance / 1000

    var nonce = 1
    forAll(paymentGen) { r: SimplePayment =>
      val p = r.copy(sender = genesisAcc.publicImage, nonce = nonce, fee = r.fee % Mod, amount = r.amount % Mod)
      whenever(state.validate(p).isSuccess) {
        val block = SimpleBlock(ModifierId @@ Array.fill(SimpleBlock.SignatureLength)(-1: Byte),
          0L,
          GenerationSignature @@ Array.fill(SimpleBlock.SignatureLength)(0: Byte),
          BaseTarget @@ 1L,
          genesisAcc.publicImage,
          Seq(p))

        state.applyModifier(block) match {
          case Success(newState) =>
            state = newState
            nonce = nonce + 1
            state.totalBalance shouldBe GenesisBalance
          case Failure(e) =>
            throw e
        }
      }
    }
  }
}
