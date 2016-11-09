package examples.curvepos.serialization

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import examples.curvepos.transaction.{SimpleBlock, SimplePayment}
import scorex.core.block.Block
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

class SimpleBlockSerializer extends ScorexSerializer[SimpleBlock] {
  override def write(kryo: Kryo, output: Output, block: SimpleBlock): Unit = {
    output.writeBytes(block.parentId)
    output.writeLong(block.timestamp)
    output.writeByte(block.version)
    output.writeBytes(block.generationSignature)
    output.writeLong(block.baseTarget)
    output.writeBytes(block.generator.pubKeyBytes)
    output.writeInt(block.txs.size)
    block.txs.foreach { tx => kryo.writeObject(output, tx, new SimplePaymentSerializer) }
  }

  override def read(kryo: Kryo, input: Input, c: Class[SimpleBlock]): SimpleBlock = {
    val parentId = input.readBytes(Block.BlockIdLength)
    val timestamp = input.readLong()
    val version = input.readByte()
    val generationSignature = input.readBytes(SimpleBlock.SignatureLength)
    val baseTarget = input.readLong()
    val generator = PublicKey25519Proposition(input.readBytes(32))
    val cnt = input.readInt()
    val txs = (0 until cnt) map { i =>
      kryo.readObject(input, classOf[SimplePayment], new SimplePaymentSerializer())
    }
    SimpleBlock(parentId, timestamp, generationSignature, baseTarget, generator, txs)

  }
}
