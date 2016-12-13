package hybrid.state

import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.mining.MiningSettings
import examples.hybrid.state.HBoxStoredState
import hybrid.HybridGenerators
import io.circe
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.StateChanges

import scala.util.Random

class HBoxStoredStateSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {

  val settings = new Settings with MiningSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("settings.json")
  }

  var state = HBoxStoredState.readOrGenerate(settings)


  property("State should be able to add a box") {
    forAll(noncedBoxGen) { b =>
      val c = StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](Set(), Set(b))

      state = state.applyChanges(c, Array.fill(32)(Random.nextInt(Byte.MaxValue).toByte)).get

      state.closedBox(b.id).isDefined shouldBe true
    }
  }

}
