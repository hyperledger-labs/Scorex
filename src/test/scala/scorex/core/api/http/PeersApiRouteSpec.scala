package scorex.core.api.http

import java.net.InetSocketAddress

import akka.http.scaladsl.model.{ContentTypes, StatusCodes, HttpEntity}
import akka.http.scaladsl.testkit.{ScalatestRouteTest, RouteTestTimeout}
import akka.testkit.TestDuration
import io.circe.Json
import io.circe.syntax._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.api.http.PeersApiRoute.PeerInfoResponse
import scorex.core.settings.{RESTApiSettings, ScorexSettings}
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.duration._
import scala.language.postfixOps

class PeersApiRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs {

  implicit val timeout: RouteTestTimeout = RouteTestTimeout(15.seconds dilated)

  private val addr = new InetSocketAddress("localhost", 8080)
  private val restApiSettings = RESTApiSettings(addr, None, None, 10 seconds)
  private val prefix = "/peers"
  private val settings = ScorexSettings.read(None)
  private val timeProvider = new NetworkTimeProvider(settings.ntp)
  private val routes = PeersApiRoute(pmRef, networkControllerRef, timeProvider, restApiSettings).route

  val peersResp: String = peers.map { case (address, peerInfo) =>
    PeerInfoResponse.fromAddressAndInfo(address, peerInfo).asJson
  }.asJson.toString

  val connectedPeersResp: Json = connectedPeers.map { handshake =>
    Map(
      "address" -> handshake.peerSpec.declaredAddress.toString.asJson,
      "name" -> handshake.peerSpec.nodeName.asJson,
      "lastSeen" -> handshake.time.asJson
    ).asJson
  }.asJson

  it should "get all peers" in {
    Get(prefix + "/all") ~> routes ~> check {
      status shouldBe StatusCodes.OK
      peersResp shouldBe responseAs[String]
    }
  }

  //can't check it cause original node using now() timestamp for last seen field
  ignore should "get connected peers" in {
    Get(prefix + "/connected") ~> routes ~> check {
      status shouldBe StatusCodes.OK
      connectedPeersResp shouldBe responseAs[String]
    }
  }

  it should "connect to peer" in {
    val body = HttpEntity("localhost:8080".asJson.toString).withContentType(ContentTypes.`application/json`)
    Post(prefix + "/connect", body) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "get blacklisted peers" in {
    Get(prefix + "/blacklisted") ~> routes ~> check {
      status shouldBe StatusCodes.OK
      Map("addresses" -> blacklistedPeers.map(_.toString)).asJson.toString shouldBe responseAs[String]
    }
  }

}
