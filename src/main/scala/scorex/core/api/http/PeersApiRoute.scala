package scorex.core.api.http

import java.net.{InetAddress, InetSocketAddress}
import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.JsonObject
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.network.Handshake
import scorex.core.network.NetworkController.ConnectTo
import scorex.core.network.peer.{PeerInfo, PeerManager}
import scorex.core.settings.RESTApiSettings

import scala.concurrent.ExecutionContext.Implicits.global

@Path("/peers")
@Api(value = "/peers", description = "Get info about peers", position = 2)
case class PeersApiRoute(peerManager: ActorRef,
                         networkController: ActorRef,
                         override val settings: RESTApiSettings)(implicit val context: ActorRefFactory)
  extends ApiRoute {

  override lazy val route =
    pathPrefix("peers") {
      allPeers ~ connectedPeers ~ blacklistedPeers ~ connect
    }

  @Path("/all")
  @ApiOperation(value = "Peer list", notes = "Peer list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def allPeers: Route = path("all") {
    getJsonRoute {
      (peerManager ? PeerManager.GetAllPeers)
        .mapTo[Map[InetSocketAddress, PeerInfo]]
        .map { peers =>
          peers.map { case (address, peerInfo) =>
            Seq(
              Some("address" -> address.toString),
              Some("lastSeen" -> peerInfo.lastSeen),
              peerInfo.nodeName.map(name => "name" -> name),
              peerInfo.nonce.map(nonce => "nonce" -> nonce)).flatten.toMap
          }.asJson
        }.map(s => SuccessApiResponse(s))
    }
  }

  @Path("/connected")
  @ApiOperation(value = "Connected peers list", notes = "Connected peers list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with connected peers or error")
  ))
  def connectedPeers: Route = path("connected") {
    getJsonRoute {
      val now = System.currentTimeMillis()
      (peerManager ? PeerManager.GetConnectedPeers)
        .mapTo[Seq[Handshake]]
        .map { handshakes =>
          handshakes.map { handshake =>
            Map(
              "address" -> handshake.declaredAddress.toString,
              "name" -> handshake.nodeName,
              "nonce" -> handshake.nodeNonce,
              "lastSeen" -> now
            ).asJson
          }.asJson
        }.map(s => SuccessApiResponse(s))
    }
  }

  private case class ConnectCommandParams(host: String, port: Int)

  @Path("/connect")
  @ApiOperation(value = "Connect to peer", notes = "Connect to peer", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      defaultValue = "{\n\t\"host\":\"127.0.0.1\",\n\t\"port\":\"9084\"\n}"
    )
  )) def connect: Route = path("connect") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          decode[ConnectCommandParams](body) match {
            case Right(ConnectCommandParams(host, port)) =>
              val add: InetSocketAddress = new InetSocketAddress(InetAddress.getByName(host), port)
              networkController ! ConnectTo(add)
              SuccessApiResponse(JsonObject.empty.asJson)
            case _ =>
              ApiError.wrongJson
          }
        }
      }
    }
  }

  @Path("/blacklisted")
  @ApiOperation(value = "Blacklisted peers list", notes = "Connected peers list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with connected peers or error")
  ))
  def blacklistedPeers: Route = path("blacklisted") {
    getJsonRoute {
      (peerManager ? PeerManager.GetBlacklistedPeers)
        .mapTo[Seq[String]]
        .map(s => SuccessApiResponse(Map("address" -> s).asJson))
    }
  }
}
