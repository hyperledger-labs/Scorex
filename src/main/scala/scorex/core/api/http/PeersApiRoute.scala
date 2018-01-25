package scorex.core.api.http

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import scorex.core.network.Handshake
import scorex.core.network.NetworkController.ConnectTo
import scorex.core.network.peer.{PeerInfo, PeerManager}
import scorex.core.settings.RESTApiSettings

import scala.concurrent.ExecutionContext.Implicits.global

case class PeersApiRoute(peerManager: ActorRef,
                         networkController: ActorRef,
                         override val settings: RESTApiSettings)(implicit val context: ActorRefFactory)
  extends ApiRoute {

  override lazy val route =
    pathPrefix("peers") {
      allPeers ~ connectedPeers ~ blacklistedPeers ~ connect
    }

  def allPeers: Route = path("all") {
    getJsonRoute {
      (peerManager ? PeerManager.GetAllPeers)
        .mapTo[Map[InetSocketAddress, PeerInfo]]
        .map { peers =>
          peers.map { case (address, peerInfo) =>
            Seq(
              Some("address" -> address.toString.asJson),
              Some("lastSeen" -> peerInfo.lastSeen.asJson),
              peerInfo.nodeName.map(name => "name" -> name.asJson)).flatten.toMap
          }.asJson
        }.map(s => SuccessApiResponse(s))
    }
  }

  def connectedPeers: Route = path("connected") {
    getJsonRoute {
      val now = System.currentTimeMillis()
      (peerManager ? PeerManager.GetConnectedPeers)
        .mapTo[Seq[Handshake]]
        .map { handshakes =>
          handshakes.map { handshake =>
            Map(
              "address" -> handshake.declaredAddress.toString.asJson,
              "name" -> handshake.nodeName.asJson,
              "lastSeen" -> now.asJson
            ).asJson
          }.asJson
        }.map(s => SuccessApiResponse(s))
    }
  }

  private val addressAndPortRegexp = "\\w+:\\d{1,5}".r

  def connect: Route = path("connect") {
    post {
      withAuth {
        entity(as[String]) { body =>
          complete {
            if (addressAndPortRegexp.findFirstMatchIn(body).isDefined) {
              val Array(host, port) = body.split(":")
              val add: InetSocketAddress = new InetSocketAddress(InetAddress.getByName(host), port.toInt)
              networkController ! ConnectTo(add)
              StatusCodes.OK
            } else {
              StatusCodes.BadRequest
            }
          }
        }
      }
    }
  }

  def blacklistedPeers: Route = path("blacklisted") {
    getJsonRoute {
      (peerManager ? PeerManager.GetBlacklistedPeers)
        .mapTo[Seq[String]]
        .map(s => SuccessApiResponse(Map("address" -> s).asJson))
    }
  }
}
