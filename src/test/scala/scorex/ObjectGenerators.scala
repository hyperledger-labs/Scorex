package scorex

import java.net.{InetAddress, InetSocketAddress}

import org.scalacheck.{Arbitrary, Gen}
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.app.ApplicationVersion
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.signatures.Curve25519

trait ObjectGenerators {
  lazy val nonEmptyBytesGen: Gen[Array[Byte]] = Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
    .map(_.toArray).suchThat(_.length > 0)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }
  }

  def genBytesList(size: Int): Gen[Array[Byte]] = genBoundedBytes(size, size)

  lazy val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)


  lazy val modifierIdGen: Gen[ModifierId] =
    Gen.listOfN(NodeViewModifier.ModifierIdSize, Arbitrary.arbitrary[Byte]).map(_.toArray)

  lazy val invDataGen: Gen[InvData] = for {
    modifierTypeId: Byte <- Arbitrary.arbitrary[Byte]
    modifierIds: Seq[Array[Byte]] <- Gen.nonEmptyListOf(modifierIdGen) if modifierIds.nonEmpty
  } yield modifierTypeId -> modifierIds

  lazy val modifierWithIdGen: Gen[(ModifierId, Array[Byte])] = for {
    id <- modifierIdGen
    mod <- nonEmptyBytesGen
  } yield id -> mod

  lazy val modifiersGen: Gen[ModifiersData] = for {
    modifierTypeId: Byte <- Arbitrary.arbitrary[Byte]
    modifiers: Map[ModifierId, Array[Byte]] <- Gen.nonEmptyMap(modifierWithIdGen).suchThat(_.nonEmpty)
  } yield modifierTypeId -> modifiers

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
