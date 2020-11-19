package scorex.testkit.properties

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.wallet.BoxWallet

trait WalletSecretsTest[P <: Proposition, TX <: Transaction, PM <: PersistentNodeViewModifier]
  extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with Matchers {

  val wallet: BoxWallet[P, TX, PM, _]

  property("Wallet should contain secrets for all it's public propositions") {
    val publicImages = wallet.publicKeys
    assert(publicImages.nonEmpty, "please provide wallet with at least one secret")
    publicImages.foreach(pi => wallet.secretByPublicImage(pi).isDefined shouldBe true)
  }
}
