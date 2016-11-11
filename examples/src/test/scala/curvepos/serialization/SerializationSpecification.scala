package curvepos.serialization


import curvepos.ExampleGenerators
import examples.curvepos.SimpleSyncInfo
import examples.curvepos.serialization.CurveposRegistrar
import examples.curvepos.transaction.{PublicKey25519NoncedBox, SimpleBlock, SimplePayment}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.core.serialization.{ScorexKryoPool, ScorexRegistrar}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.wallet.WalletBox

import scala.reflect.ClassTag

class SerializationSpecification extends PropSpec with GeneratorDrivenPropertyChecks with Matchers
with ExampleGenerators {

  private val pool = new ScorexKryoPool(new ScorexRegistrar, new CurveposRegistrar)

  property("SimplePayment serialization roundtrip") {
    checkSerialization[SimplePayment](paymentGen)
  }

  property("SimpleBlock serialization roundtrip") {
    checkSerialization[SimpleBlock](blockGenerator)
  }

  property("SimpleSyncInfo serialization roundtrip") {
    checkSerialization[SimpleSyncInfo](simpleSyncInfoGenerator, emptyCheck[SimpleSyncInfo])
  }

  property("PublicKey25519NoncedBox serialization roundtrip") {
    checkSerialization[PublicKey25519NoncedBox](publicKey25519NoncedBoxGen, emptyCheck[PublicKey25519NoncedBox])
  }

  property("WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox] serialization roundtrip") {
    checkSerialization[WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox]](walletBoxGen,
      emptyCheck[WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox]])
  }


  def emptyCheck[T](o1: T, o2: T): Unit = {}

  def checkSerialization[T: ClassTag](gen: Gen[T], check: (T, T) => Unit = (o1: T, o2: T) => o1 shouldBe o2) = {
    forAll(gen) { obj: T =>
      val bytes = pool.toBytes(obj)
      val obj2 = pool.fromBytes[T](bytes).get
      check(obj2, obj)
      val bytes2 = pool.toBytes(obj2)
      bytes shouldEqual bytes2
    }
  }
}
