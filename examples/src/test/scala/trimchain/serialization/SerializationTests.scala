package trimchain.serialization

import examples.trimchain.core.{Ticket, TicketSerializer}
import examples.trimchain.modifiers.{BlockHeader, BlockHeaderSerializer, TBlock, TBlockSerializer}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import trimchain.TrimchainGenerators

class SerializationTests extends AnyPropSpec
  with ScalaCheckPropertyChecks
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
