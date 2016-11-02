package hybrid.serialization

import examples.hybrid.blocks.PosBlock
import examples.hybrid.history.HybridSyncInfo
import examples.hybrid.state.SimpleBoxTransaction
import hybrid.HybridGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class SerializationTests extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers
with HybridGenerators {

  property("PosBlock serialization") {
    forAll(posBlockGen) { b: PosBlock =>
      val parsed = b.companion.parse(b.bytes).get
      parsed.bytes shouldEqual b.bytes
    }
  }

  property("SimpleBoxTransaction serialization") {
    forAll(simpleBoxTransactionGen) { b: SimpleBoxTransaction =>
      val parsed = b.companion.parse(b.bytes).get
      parsed.bytes shouldEqual b.bytes
    }
  }

  property("HybridSyncInfo serialization") {
    forAll(hybridSyncInfoGen) { b: HybridSyncInfo =>
      val parsed = HybridSyncInfo.parse(b.bytes).get
      parsed.bytes shouldEqual b.bytes
    }
  }

}
