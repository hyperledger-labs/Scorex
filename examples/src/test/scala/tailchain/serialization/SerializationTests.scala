package tailchain.serialization

import examples.tailchain.core.{PartialProof, PartialProofSerializer, Ticket, TicketSerializer}
import examples.tailchain.modifiers.{BlockHeader, BlockHeaderSerializer, TBlock, TBlockSerializer}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import tailchain.TailchainGenerators

class SerializationTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TailchainGenerators {

  property("PartialProof serialization") {
    forAll(partialProofGen) { b: PartialProof =>
      val serializer = PartialProofSerializer
      val parsed = serializer.parseBytes(serializer.toBytes(b)).get
      serializer.toBytes(b) shouldEqual serializer.toBytes(parsed)
    }
  }

  property("Ticket serialization") {
    forAll(ticketGen) { b: Ticket =>
      val serializer = TicketSerializer
      val parsed = serializer.parseBytes(serializer.toBytes(b)).get
      b.minerKey shouldEqual parsed.minerKey
      serializer.toBytes(b) shouldEqual serializer.toBytes(parsed)
    }
  }

  property("BlockHeader serialization") {
    forAll(blockHeaderGen) { b: BlockHeader =>
      val serializer = BlockHeaderSerializer
      val parsed = serializer.parseBytes(serializer.toBytes(b)).get
      serializer.toBytes(b) shouldEqual serializer.toBytes(parsed)
    }
  }

  property("TBlock serialization") {
    forAll(TBlockGen) { b: TBlock =>
      val serializer = TBlockSerializer
      val parsed = serializer.parseBytes(serializer.toBytes(b)).get
      serializer.toBytes(b) shouldEqual serializer.toBytes(parsed)
    }
  }



}
