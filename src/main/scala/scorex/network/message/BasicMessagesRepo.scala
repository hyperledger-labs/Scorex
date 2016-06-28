package scorex.network.message

import java.net.{InetAddress, InetSocketAddress}
import java.util

import com.google.common.primitives.{Bytes, Ints}
import scorex.block.{Block, ConsensusData, TransactionalData}
import scorex.consensus.ConsensusModule
import scorex.crypto.signatures.SigningFunctions
import scorex.network.message.Message._
import scorex.transaction.TransactionModule
import scorex.transaction.box.Proposition
import scorex.transaction.proof.Signature25519

import scala.util.Try


class BasicMessagesRepo[P <: Proposition, CData <: ConsensusData, TData <: TransactionalData[_], B <: Block[P, CData, TData]]()
                                                                              (implicit val transactionalModule: TransactionModule[P, _, TData],
                                                                               consensusModule: ConsensusModule[P, CData, B]) {

  type BlockId = ConsensusData.BlockId

  object GetPeersSpec extends MessageSpec[Unit] {
    override val messageCode: Message.MessageCode = 1: Byte

    override val messageName: String = "GetPeers message"

    override def deserializeData(bytes: Array[Byte]): Try[Unit] =
      Try(require(bytes.isEmpty, "Non-empty data for GetPeers"))

    override def serializeData(data: Unit): Array[Byte] = Array()
  }

  object PeersSpec extends MessageSpec[Seq[InetSocketAddress]] {
    private val AddressLength = 4
    private val PortLength = 4
    private val DataLength = 4

    override val messageCode: Message.MessageCode = 2: Byte

    override val messageName: String = "Peers message"

    override def deserializeData(bytes: Array[Byte]): Try[Seq[InetSocketAddress]] = Try {
      val lengthBytes = util.Arrays.copyOfRange(bytes, 0, DataLength)
      val length = Ints.fromByteArray(lengthBytes)

      assert(bytes.length == DataLength + (length * (AddressLength + PortLength)), "Data does not match length")

      (0 until length).map { i =>
        val position = lengthBytes.length + (i * (AddressLength + PortLength))
        val addressBytes = util.Arrays.copyOfRange(bytes, position, position + AddressLength)
        val address = InetAddress.getByAddress(addressBytes)
        val portBytes = util.Arrays.copyOfRange(bytes, position + AddressLength, position + AddressLength + PortLength)
        new InetSocketAddress(address, Ints.fromByteArray(portBytes))
      }
    }

    override def serializeData(peers: Seq[InetSocketAddress]): Array[Byte] = {
      val length = peers.size
      val lengthBytes = Ints.toByteArray(length)

      peers.foldLeft(lengthBytes) { case (bs, peer) =>
        Bytes.concat(bs, peer.getAddress.getAddress, Ints.toByteArray(peer.getPort))
      }
    }
  }

  trait SignaturesSeqSpec extends MessageSpec[Seq[SigningFunctions.Signature]] {

    private val SignatureLength = Signature25519.SignatureSize
    private val DataLength = 4

    override def deserializeData(bytes: Array[Byte]): Try[Seq[SigningFunctions.Signature]] = Try {
      val lengthBytes = bytes.take(DataLength)
      val length = Ints.fromByteArray(lengthBytes)

      assert(bytes.length == DataLength + (length * SignatureLength), "Data does not match length")

      (0 to length - 1).map { i =>
        val position = DataLength + (i * SignatureLength)
        bytes.slice(position, position + SignatureLength)
      }.toSeq
    }

    override def serializeData(signatures: Seq[SigningFunctions.Signature]): Array[Byte] = {
      val length = signatures.size
      val lengthBytes = Ints.toByteArray(length)

      //WRITE SIGNATURES
      signatures.foldLeft(lengthBytes) { case (bs, header) => Bytes.concat(bs, header) }
    }
  }

  object GetSignaturesSpec extends SignaturesSeqSpec {
    override val messageCode: MessageCode = 20: Byte
    override val messageName: String = "GetSignatures message"
  }

  object SignaturesSpec extends SignaturesSeqSpec {
    override val messageCode: MessageCode = 21: Byte
    override val messageName: String = "Signatures message"
  }

  object GetBlockSpec extends MessageSpec[BlockId] {
    override val messageCode: MessageCode = 22: Byte
    override val messageName: String = "GetBlock message"

    override def serializeData(id: BlockId): Array[Byte] = id

    override def deserializeData(bytes: Array[Byte]): Try[BlockId] = Try {
      require(bytes.length == consensusModule.BlockIdLength, "Data does not match length")
      bytes
    }
  }

  object BlockMessageSpec extends MessageSpec[B] {
    override val messageCode: MessageCode = 23: Byte

    override val messageName: String = "Block message"

    override def serializeData(block: B): Array[Byte] = block.bytes

    override def deserializeData(bytes: Array[Byte]): Try[B] = Block.parse(bytes)(consensusModule, transactionalModule)
  }

  object ScoreMessageSpec extends MessageSpec[BigInt] {
    override val messageCode: MessageCode = 24: Byte

    override val messageName: String = "Score message"

    override def serializeData(score: BigInt): Array[Byte] = {
      val scoreBytes = score.toByteArray
      val bb = java.nio.ByteBuffer.allocate(scoreBytes.length)
      bb.put(scoreBytes)
      bb.array()
    }

    override def deserializeData(bytes: Array[Byte]): Try[BigInt] = Try {
      BigInt(1, bytes)
    }
  }

  val specs = Seq(GetPeersSpec, PeersSpec, GetSignaturesSpec, SignaturesSpec,
    GetBlockSpec, BlockMessageSpec, ScoreMessageSpec)
}