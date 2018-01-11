package scorex.core.api.http

import java.security.SecureRandom

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import scorex.core.settings.RESTApiSettings
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256


case class UtilsApiRoute(override val settings: RESTApiSettings)(implicit val context: ActorRefFactory) extends ApiRoute {
  val SeedSize = 32

  private def seed(length: Int): String = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Base58.encode(seed)
  }

  override val route = pathPrefix("utils") {
    seedRoute ~ length ~ hashBlake2b
  }

  def seedRoute: Route = path("seed") {
    get {
      complete(seed(SeedSize))
    }
  }

  def length: Route = path("seed" / IntNumber) { case length =>
    get {
      complete(seed(length))
    }
  }

  def hashBlake2b: Route = {
    path("hash" / "blake2b") {
      post {
        entity(as[String]) { message =>
          complete(Base58.encode(Blake2b256(message)))
        }
      }
    }
  }
}