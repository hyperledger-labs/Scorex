package scorex.core.network

import java.net.{InetAddress, InetSocketAddress}

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.ObjectGenerators
import scorex.core.app.Version


class HandshakeSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers
with ObjectGenerators {

  val validNumbers =
    for (n <- Gen.choose(Integer.MIN_VALUE + 1, Integer.MAX_VALUE)) yield n

  property("handshake should remain the same after serialization/deserialization") {
    forAll(Gen.alphaStr, appVersionGen, Gen.alphaStr, inetSocketAddressGen, Gen.posNum[Long], Gen.posNum[Long]) {
      (appName: String,
       av: Version,
       nodeName: String,
       isa: InetSocketAddress,
       nonce: Long,
       time: Long) =>

        whenever(appName.nonEmpty) {

          val h1 = Handshake(appName, av, nodeName, nonce, None, time)
          val hr1: Handshake = HandshakeSerializer.parseBytes(h1.bytes).get
          hr1.applicationName should be(h1.applicationName)
          hr1.protocolVersion should be(h1.protocolVersion)
          hr1.declaredAddress should be(h1.declaredAddress)
          hr1.nodeNonce should be(h1.nodeNonce)
          hr1.time should be(h1.time)

          val h2 = Handshake(appName, av, nodeName, nonce, Some(isa), time)
          val hr2 = HandshakeSerializer.parseBytes(h2.bytes).get
          hr2.applicationName should be(h2.applicationName)
          hr2.protocolVersion should be(h2.protocolVersion)
          hr2.declaredAddress should be(h2.declaredAddress)
          hr2.nodeNonce should be(h2.nodeNonce)
          hr2.time should be(h2.time)
        }
    }
  }
}