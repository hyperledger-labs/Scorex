package scorex

import java.net.{InetAddress, InetSocketAddress}

import org.scalacheck.{Arbitrary, Gen}
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.app.ApplicationVersion
import scorex.core.network.Handshake
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.signatures.Curve25519
import scorex.crypto.signatures.Curve25519._

trait ObjectGenerators {
  lazy val nonEmptyBytesGen: Gen[Array[Byte]] = Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
    .map(_.toArray).suchThat(_.length > 0)

  def sizedBytesGen(n: Int = SignatureLength): Gen[Array[Byte]] =
    Gen.listOfN(n, Arbitrary.arbitrary[Byte]).map(_.toArray)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }
  }

  def genBytesList(size: Int): Gen[Array[Byte]] = genBoundedBytes(size, size)

  lazy val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)


  lazy val modifierIdGen: Gen[ModifierId] = genBytesList(NodeViewModifier.ModifierIdSize)

  lazy val invDataGen: Gen[InvData] = for {
    modifierTypeId: Byte <- Arbitrary.arbitrary[Byte]
    modifierIds: Seq[Array[Byte]] <- Gen.nonEmptyListOf(modifierIdGen)
  } yield InvData(modifierTypeId, modifierIds)

  lazy val modifierWithIdGen: Gen[(ModifierId, Array[Byte])] = for {
    id <- modifierIdGen
    mod <- nonEmptyBytesGen
  } yield id -> mod

  lazy val modifiersDataGen: Gen[ModifiersData] = for {
    modifierTypeId: Byte <- Arbitrary.arbitrary[Byte]
    modifiers: Map[ModifierId, Array[Byte]] <- Gen.nonEmptyMap(modifierWithIdGen)
  } yield ModifiersData(modifierTypeId, modifiers)

  lazy val inetAddressGen: Gen[InetSocketAddress] = for {
    port <- Gen.choose(1, 0xFFFF)
    address <- sizedBytesGen(4)
  } yield new InetSocketAddress(InetAddress.getByAddress(address), port)

  lazy val peersDataGen: Gen[PeersData] = Gen.listOf(inetAddressGen).map(p => PeersData(p))

  lazy val inetAddressOptGen: Gen[Option[InetSocketAddress]] = Gen.option(inetAddressGen)

  lazy val handshakeGen: Gen[Handshake] = for {
    applicationName <- Arbitrary.arbitrary[String] withFilter (_.length < 80) withFilter (_.length > 0)
    nodeName <- Arbitrary.arbitrary[String] withFilter (_.length < 80)
    nodeNonce <- Arbitrary.arbitrary[Long]
    time <- Arbitrary.arbitrary[Long]
    address <- inetAddressOptGen
    applicationVersion <- appVersionGen
  } yield Handshake(applicationName, applicationVersion, nodeName, nodeNonce, address, time)

  val MaxVersion = 999
  val MaxIp = 255
  val MaxPort = 65535

  lazy val appVersionGen = for {
    fd <- Gen.choose(0, MaxVersion)
    sd <- Gen.choose(0, MaxVersion)
    td <- Gen.choose(0, MaxVersion)
  } yield ApplicationVersion(fd, sd, td)

  lazy val inetSocketAddressGen = for {
    ip1 <- Gen.choose(0, MaxIp)
    ip2 <- Gen.choose(0, MaxIp)
    ip3 <- Gen.choose(0, MaxIp)
    ip4 <- Gen.choose(0, MaxIp)
    port <- Gen.choose(0, MaxPort)
  } yield new InetSocketAddress(InetAddress.getByName(s"$ip1.$ip2.$ip3.$ip4"), port)

  lazy val propositionGen: Gen[PublicKey25519Proposition] = genBytesList(Curve25519.KeyLength)
    .map(s => PrivateKey25519Companion.generateKeys(s)._2)
}
