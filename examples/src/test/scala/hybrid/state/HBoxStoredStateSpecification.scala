package hybrid.state

import examples.hybrid.blocks.HybridBlock
import examples.hybrid.state.HBoxStoredState
import hybrid.HybridGenerators
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.core.transaction.state.{Insertion, Removal}
import scorex.testkit.properties.state.StateTests

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class HBoxStoredStateSpecification extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with HybridGenerators
  with StateTests[HybridBlock, HBoxStoredState] {

  property("added boxes are always there") {
    forAll(stateGen){state =>
      var st = state
      check(checksToMake) { _ =>
        val c = stateChangesGenerator(state).sample.get
        st = st.applyChanges(c, versionTagGen.sample.get).get
        c.toAppend.foreach { case Insertion(b) =>
            st.closedBox(b.id) shouldBe Some(b)
        }
        c.toRemove.foreach { case Removal(bid) =>
          st.closedBox(bid) shouldBe None
        }
      }
    }
  }
}
