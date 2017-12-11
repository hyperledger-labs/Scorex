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


@Path("/utils")
@Api(value = "/utils", description = "Useful functions", position = 3, produces = "plain/text")
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

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Generate random seed", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Seed string")
  ))
  def seedRoute: Route = path("seed") {
    get {
      complete(seed(SeedSize))
    }
  }

  @Path("/seed/{length}")
  @ApiOperation(value = "Seed of specified length", notes = "Generate random seed of specified length", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "length", value = "Seed length ", required = true, dataType = "long", paramType = "path")
  ))
  @ApiResponse(code = 200, message = "Seed string")
  def length: Route = path("seed" / IntNumber) { case length =>
    get {
      complete(seed(length))
    }
  }

  @Path("/hash/blake2b")
  @ApiOperation(value = "Hash", notes = "Return Blake2b hash of specified message", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to hash", required = true, paramType = "body", dataType = "String")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "hash value")
  ))
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