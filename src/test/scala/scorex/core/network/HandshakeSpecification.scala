package scorex.core.network

import java.net.InetSocketAddress

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.ObjectGenerators
import scorex.core.app.Version
import scorex.core.network.message.HandshakeSpec


class HandshakeSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ObjectGenerators {

  private val featSerializers = Map(FullNodePeerFeature.featureId -> FullNodePeerFeature.serializer)
  private val noSerializers: PeerFeature.Serializers = Map()

  property("handshake should remain the same after serialization/deserialization") {
    val serializersGen = Gen.oneOf(featSerializers, noSerializers)
    val featuresGen: Gen[Seq[PeerFeature]] = Gen.oneOf(Seq(FullNodePeerFeature), Seq[PeerFeature]())

    forAll(Gen.alphaStr, appVersionGen, Gen.alphaStr, Gen.option(inetSocketAddressGen), Gen.posNum[Long], serializersGen) {
      (appName: String,
       av: Version,
       nodeName: String,
       isaOpt: Option[InetSocketAddress],
       time: Long,
       serializers: PeerFeature.Serializers) =>

        whenever(appName.nonEmpty) {
          val feats: Seq[PeerFeature] = featuresGen.sample.getOrElse(Seq())

          val handshakeSerializer = new HandshakeSpec(serializers)

          val h = Handshake(appName, av, nodeName, isaOpt, feats, time)
          val hr = handshakeSerializer.parseBytes(handshakeSerializer.toBytes(h))
          hr.peerData.agentName should be(h.peerData.agentName)
          hr.peerData.protocolVersion should be(h.peerData.protocolVersion)
          hr.peerData.declaredAddress should be(h.peerData.declaredAddress)
          if (serializers.nonEmpty) hr.peerData.features shouldBe h.peerData.features else hr.peerData.features.isEmpty shouldBe true
          hr.time should be(h.time)
        }
    }
  }
}