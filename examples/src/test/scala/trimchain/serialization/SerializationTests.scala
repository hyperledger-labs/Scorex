package trimchain.serialization

import examples.trimchain.core.{Ticket, TicketSerializer}
import examples.trimchain.modifiers.{BlockHeader, BlockHeaderSerializer, TBlock, TBlockSerializer}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import trimchain.TrimchainGenerators

class SerializationTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TrimchainGenerators {

  property("Ticket serialization") {
    forAll(ticketGen) { b: Ticket =>
      val serializer = TicketSerializer
      val parsed = serializer.parseBytes(serializer.toBytes(b))
      b.minerKey shouldEqual parsed.minerKey
      serializer.toBytes(b) shouldEqual serializer.toBytes(parsed)
    }
  }

  property("BlockHeader serialization") {
    forAll(blockHeaderGen) { b: BlockHeader =>
      val serializer = BlockHeaderSerializer
      val parsed = serializer.parseBytes(serializer.toBytes(b))
      serializer.toBytes(b) shouldEqual serializer.toBytes(parsed)
    }
  }

  property("TBlock serialization") {
    forAll(TBlockGen) { b: TBlock =>
      val serializer = TBlockSerializer
      val parsed = serializer.parseBytes(serializer.toBytes(b))
      serializer.toBytes(b) shouldEqual serializer.toBytes(parsed)
    }
  }
}
