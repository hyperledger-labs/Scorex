package scorex

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.ActorRef
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.app.Version
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.network.peer.PeerInfo
import scorex.core.network.{ConnectedPeer, Handshake, Outgoing, PeerFeature}
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.crypto.signatures.Curve25519
import scorex.util.{ModifierId, bytesToId}

import scala.util.Try

trait ObjectGenerators {

  object FullNodePeerFeature extends PeerFeature {
    override type M = PeerFeature
    override val featureId: PeerFeature.Id = 1: Byte

    override def serializer: Serializer[PeerFeature] = new Serializer[PeerFeature] {
      override def toBytes(obj: PeerFeature): Array[Byte] = Array(1: Byte, 2: Byte, 3: Byte)

      override def parseBytes(bytes: Array[Byte]): Try[PeerFeature] = Try {
        require(bytes(0) == 1 && bytes(1) == 2 && bytes(2) == 3)
        FullNodePeerFeature
      }
    }
  }

  val MaxVersion = 999
  val MaxIp = 255
  val MaxPort = 65535

  lazy val smallInt: Gen[Int] = Gen.choose(0, 20)

  lazy val nonEmptyBytesGen: Gen[Array[Byte]] = Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
    .map(_.toArray).suchThat(_.length > 0)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }
  }

  def genBytes(size: Int): Gen[Array[Byte]] = genBoundedBytes(size, size)

  lazy val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)

  lazy val positiveByteGen: Gen[Byte] = Gen.choose(1, Byte.MaxValue)


  lazy val modifierIdGen: Gen[ModifierId] = Gen.listOfN(NodeViewModifier.ModifierIdSize, Arbitrary.arbitrary[Byte])
    .map(id => bytesToId(id.toArray))

  lazy val modifierTypeIdGen: Gen[ModifierTypeId] = Arbitrary.arbitrary[Byte].map(t => ModifierTypeId @@ t)

  lazy val invDataGen: Gen[InvData] = for {
    modifierTypeId: ModifierTypeId <- modifierTypeIdGen
    modifierIds: Seq[ModifierId] <- Gen.nonEmptyListOf(modifierIdGen) if modifierIds.nonEmpty
  } yield modifierTypeId -> modifierIds

  lazy val modifierWithIdGen: Gen[(ModifierId, Array[Byte])] = for {
    id <- modifierIdGen
    mod <- nonEmptyBytesGen
  } yield id -> mod

  lazy val modifiersGen: Gen[ModifiersData] = for {
    modifierTypeId: ModifierTypeId <- modifierTypeIdGen
    modifiers: Map[ModifierId, Array[Byte]] <- Gen.nonEmptyMap(modifierWithIdGen).suchThat(_.nonEmpty)
  } yield modifierTypeId -> modifiers

  lazy val appVersionGen: Gen[Version] = for {
    fd <- Gen.choose(0: Byte, Byte.MaxValue)
    sd <- Gen.choose(0: Byte, Byte.MaxValue)
    td <- Gen.choose(0: Byte, Byte.MaxValue)
  } yield Version(fd, sd, td)

  lazy val inetSocketAddressGen: Gen[InetSocketAddress] = for {
    ip1 <- Gen.choose(0, MaxIp)
    ip2 <- Gen.choose(0, MaxIp)
    ip3 <- Gen.choose(0, MaxIp)
    ip4 <- Gen.choose(0, MaxIp)
    port <- Gen.choose(0, MaxPort)
  } yield new InetSocketAddress(InetAddress.getByName(s"$ip1.$ip2.$ip3.$ip4"), port)

  lazy val key25519Gen: Gen[(PrivateKey25519, PublicKey25519Proposition)] = genBytes(Curve25519.KeyLength)
    .map(s => PrivateKey25519Companion.generateKeys(s))

  lazy val propositionGen: Gen[PublicKey25519Proposition] = key25519Gen.map(_._2)

  def connectedPeerGen(peerRef: ActorRef): Gen[ConnectedPeer] = for {
    address <- inetSocketAddressGen
  } yield ConnectedPeer(
    address,
    peerRef,
    None
  )
}
