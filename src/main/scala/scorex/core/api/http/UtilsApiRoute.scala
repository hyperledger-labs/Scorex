package scorex.core.api.http

import java.security.SecureRandom
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.syntax._
import io.swagger.annotations._
import scorex.crypto.encode.Base58
import scorex.core.settings.RESTApiSettings
import scorex.crypto.hash.Blake2b256


case class UtilsApiRoute(override val settings: RESTApiSettings)(implicit val context: ActorRefFactory) extends ApiRoute {
  val SeedSize = 32

  private def seed(length: Int): SuccessApiResponse = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    SuccessApiResponse(Map("seed" -> Base58.encode(seed)).asJson)
  }

  override val route = pathPrefix("utils") {
    seedRoute ~ length ~ hashBlake2b
  }

  def seedRoute: Route = path("seed") {
    getJsonRoute {
      seed(SeedSize)
    }
  }

  @ApiResponse(code = 200, message = "Json with peer list or error")
  def length: Route = path("seed" / IntNumber) { case length =>
    getJsonRoute {
      seed(length)
    }
  }

  def hashBlake2b: Route = {
    path("hash" / "blake2b") {
      entity(as[String]) { message =>
        withAuth {
          postJsonRoute {
            SuccessApiResponse(Map("message" -> message, "hash" -> Base58.encode(Blake2b256(message))).asJson)
          }
        }
      }
    }
  }
}