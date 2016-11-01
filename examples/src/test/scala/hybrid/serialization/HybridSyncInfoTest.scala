package hybrid.serialization

import examples.hybrid.history.HybridSyncInfo
import hybrid.HybridGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class HybridSyncInfoTest extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers
with HybridGenerators {

  property("HybridSyncInfo serialization") {
    forAll(hybridSyncInfoGen) { b: HybridSyncInfo =>
      val parsed = HybridSyncInfo.parse(b.bytes).get
      parsed.bytes shouldEqual b.bytes
    }
  }

}
