package scorex.testkit.properties

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.wallet.Wallet

trait WalletSecretsTest[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX]]
  extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with PropertyChecks {

  val wallet: Wallet[P, TX, PM, _]

  property("Wallet should contain secrets for all it's public propositions") {
    val publicImages = wallet.publicKeys
    assert(publicImages.nonEmpty, "please provide wallet with at least one secret")
    publicImages.foreach(pi => wallet.secretByPublicImage(pi).isDefined shouldBe true)
  }
}
