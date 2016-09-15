package scorex

import java.net.{InetAddress, InetSocketAddress}

import org.scalacheck.{Arbitrary, Gen}
import scorex.core.app.ApplicationVersion
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.transaction.NodeViewModifier
import scorex.core.transaction.NodeViewModifier.ModifierId

trait ObjectGenerators {
  lazy val nonEmptyBytesGen: Gen[Array[Byte]] = Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte]).map(_.toArray)

  lazy val modifierIdGen: Gen[ModifierId] =
    Gen.listOfN(NodeViewModifier.ModifierIdSize, Arbitrary.arbitrary[Byte]).map(_.toArray)

  lazy val invDataGen: Gen[InvData] = for {
    modifierTypeId: Byte <- Arbitrary.arbitrary[Byte]
    modifierIds: Seq[Array[Byte]] <- Gen.nonEmptyListOf(modifierIdGen) if modifierIds.nonEmpty
  } yield (modifierTypeId, modifierIds)

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

}
