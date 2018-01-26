package scorex.core.api.http

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import io.circe.{Encoder, Json}
import scorex.core.network.NetworkController.ConnectTo
import scorex.core.network.peer.{PeerInfo, PeerManager}
import scorex.core.network.{ConnectionType, Handshake, Incoming, Outgoing}
import scorex.core.settings.RESTApiSettings

import scala.concurrent.ExecutionContext.Implicits.global

case class PeerInfoResponse(address: String,
                            lastSeen: Long,
                            name: Option[String],
                            connectionType: Option[ConnectionType])

object PeerInfoResponse {

  def fromAddressAndInfo(address: InetSocketAddress, peerInfo: PeerInfo): PeerInfoResponse = PeerInfoResponse(
    address.toString,
    peerInfo.lastSeen,
    peerInfo.nodeName,
    peerInfo.connectionType
  )

  implicit val encodeFoo: Encoder[PeerInfoResponse] = new Encoder[PeerInfoResponse] {
    final def apply(peerInfoResponse: PeerInfoResponse): Json = {
      val e = Seq.empty[(String, Json)]
      val fields =  Seq(
        ("address", Json.fromString(peerInfoResponse.address)),
        ("lastSeen", Json.fromLong(peerInfoResponse.lastSeen))
      ) ++
        peerInfoResponse.name.fold(e)(n => Seq(("name", Json.fromString(n))))++
        peerInfoResponse.connectionType.fold(e) { c =>
        val v = c match {
          case Incoming => "incoming"
          case Outgoing => "outgoing"
        }
        Seq(("connectionType", Json.fromString(v)))
      }
      Json.obj(fields:_*)
    }
  }
}

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
        .map {
          _.map { case (address, peerInfo) =>
            PeerInfoResponse.fromAddressAndInfo(address, peerInfo).asJson
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
