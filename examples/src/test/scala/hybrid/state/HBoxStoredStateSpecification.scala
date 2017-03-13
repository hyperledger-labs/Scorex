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


}
