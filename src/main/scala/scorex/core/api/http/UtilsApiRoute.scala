package scorex.core.api.http

import java.security.SecureRandom
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe._
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.crypto.hash.{FastCryptographicHash}
import scorex.crypto.encode.Base58
import scorex.core.settings.Settings


@Path("/utils")
@Api(value = "/utils", description = "Useful functions", position = 3, produces = "application/json")
case class UtilsApiRoute(override val settings: Settings)(implicit val context: ActorRefFactory) extends ApiRoute {
  val SeedSize = 32

  private def seed(length: Int): SuccessApiResponse = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    SuccessApiResponse(Map("seed" -> Base58.encode(seed)).asJson)
  }

  override val route = pathPrefix("utils") {
    seedRoute ~ length ~ hashBlake2b
  }

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Generate random seed", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def seedRoute: Route = path("seed") {
    getJsonRoute {
      seed(SeedSize)
    }
  }

  @Path("/seed/{length}")
  @ApiOperation(value = "Seed of specified length", notes = "Generate random seed of specified length", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "length", value = "Seed length ", required = true, dataType = "long", paramType = "path")
  ))
  @ApiResponse(code = 200, message = "Json with peer list or error")
  def length: Route = path("seed" / IntNumber) { case length =>
    getJsonRoute {
      seed(length)
    }
  }

  @Path("/hash/blake2b")
  @ApiOperation(value = "Hash", notes = "Return Blake2b hash of specified message", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to hash", required = true, paramType = "body", dataType = "String")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with error or json like {\"message\": \"your message\",\"hash\": \"your message hash\"}")
  ))
  def hashBlake2b: Route = {
    path("hash" / "blake2b") {
      entity(as[String]) { message =>
        withAuth {
          postJsonRoute {
            SuccessApiResponse(Map("message" -> message, "hash" -> Base58.encode(FastCryptographicHash(message))).asJson)
          }
        }
      }
    }
  }
}