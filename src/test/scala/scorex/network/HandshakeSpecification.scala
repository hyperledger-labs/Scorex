package scorex.network

import java.net.InetSocketAddress

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.ObjectGenerators
import scorex.core.app.Version
import scorex.core.network.{Handshake, HandshakeSerializer, PeerFeature}

class HandshakeSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ObjectGenerators {

  val validNumbers: Gen[Int] =
    for (n <- Gen.choose(Integer.MIN_VALUE + 1, Integer.MAX_VALUE)) yield n

  private val feats = Seq(FullNodePeerFeature)
  private val featSerializers = Map(FullNodePeerFeature.featureId -> FullNodePeerFeature.serializer)
  private val noSerializers: PeerFeature.Serializers = Map()

  property("handshake should remain the same after serialization/deserialization") {
    val serializersGen = Gen.oneOf(featSerializers, noSerializers)

    forAll(Gen.alphaStr, appVersionGen, Gen.alphaStr, inetSocketAddressGen, Gen.posNum[Long], serializersGen) {
      (appName: String,
       av: Version,
       nodeName: String,
       isa: InetSocketAddress,
       time: Long,
       serializers) =>

        whenever(appName.nonEmpty) {

          val handshakeSerializer = new HandshakeSerializer(serializers, 2048)

          val h1 = Handshake(appName, av, nodeName, None, feats, time)
          val bs1 = handshakeSerializer.toByteString(h1)
          val hr1: Handshake = handshakeSerializer.parseByteString(bs1)
          hr1.applicationName should be(h1.applicationName)
          hr1.protocolVersion should be(h1.protocolVersion)
          hr1.declaredAddress should be(h1.declaredAddress)
          if (serializers.nonEmpty) hr1.features shouldBe h1.features else hr1.features.isEmpty shouldBe true
          hr1.time should be(h1.time)

          val h2 = Handshake(appName, av, nodeName, Some(isa), feats, time)
          val bs2 = handshakeSerializer.toByteString(h2)
          val hr2 = handshakeSerializer.parseByteString(bs2)
          hr2.applicationName should be(h2.applicationName)
          hr2.protocolVersion should be(h2.protocolVersion)
          hr2.declaredAddress should be(h2.declaredAddress)
          if (serializers.nonEmpty) hr2.features shouldBe h2.features else hr2.features.isEmpty shouldBe true
          hr2.time should be(h2.time)

          val h3 = Handshake(appName, av, nodeName, Some(isa), Seq(), time)
          val bs3 = handshakeSerializer.toByteString(h3)
          val hr3 = handshakeSerializer.parseByteString(bs3)
          hr3.applicationName should be(h3.applicationName)
          hr3.protocolVersion should be(h3.protocolVersion)
          hr3.declaredAddress should be(h3.declaredAddress)
          if (serializers.nonEmpty) hr3.features shouldBe h3.features else hr3.features.isEmpty shouldBe true
          hr3.time should be(h3.time)
        }
    }
  }
}