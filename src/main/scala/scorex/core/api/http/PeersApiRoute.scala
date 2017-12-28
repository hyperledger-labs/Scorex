package scorex.core.api.http

import java.net.{InetAddress, InetSocketAddress}
import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.swagger.annotations._
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
            Map(
              // todo get or empty
              "declaredAddress" -> address.toString,
              "nodeName" -> (peerInfo.nodeName.getOrElse("N/A"): String),
              "nodeNonce" -> (peerInfo.nonce.map(_.toString).getOrElse("N/A"): String)
            )
          }.asJson
        }.map(s => SuccessApiResponse(s))
    }
  }

  def connectedPeers: Route = path("connected") {
    getJsonRoute {
      (peerManager ? PeerManager.GetConnectedPeers)
        .mapTo[Seq[Handshake]]
        .map { handshakes =>
          val peerData = handshakes.map { handshake =>
            Map(
              // todo get or empty
              "declaredAddress" -> handshake.declaredAddress.toString,
              "nodeName" -> handshake.nodeName
            ).asJson
          }.asJson
          Map("peers" -> peerData).asJson
        }.map(s => SuccessApiResponse(s))
    }
  }

  private case class ConnectCommandParams(host: String, port: Int)

  def connect: Route = path("connect") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          decode[ConnectCommandParams](body) match {
            case Right(ConnectCommandParams(host, port)) =>
              val add: InetSocketAddress = new InetSocketAddress(InetAddress.getByName(host), port)
              networkController ! ConnectTo(add)
              SuccessApiResponse(Map("hostname" -> add.getHostName, "status" -> "Trying to connect").asJson)
            case _ =>
              ApiError.wrongJson
          }
        }
      }
    }
  }

  def blacklistedPeers: Route = path("blacklisted") {
    getJsonRoute {
      (peerManager ? PeerManager.GetBlacklistedPeers)
        .mapTo[Seq[String]]
        .map(s => SuccessApiResponse(s.asJson))
    }
  }
}
