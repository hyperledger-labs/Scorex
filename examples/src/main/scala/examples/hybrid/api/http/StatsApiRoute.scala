package examples.hybrid.api.http

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.api.http.SuccessApiResponse
import scorex.core.settings.Settings
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global


@Path("/stats")
@Api(value = "/stats", produces = "application/json")
case class StatsApiRoute(override val settings: Settings, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory) extends ApiRouteWithView {

  override val route = pathPrefix("stats") {
    tail
  }

  @Path("/tail/{length}")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "length", value = "Seed length ", required = true, dataType = "long", paramType = "path")
  ))
  @ApiOperation(value = "Tail", notes = "Return last length block ids", httpMethod = "GET")
  def tail: Route = path("tail" / IntNumber) {  count =>
    getJsonRoute {
      viewAsync().map { view =>
        SuccessApiResponse(Map(
          "count" -> count.asJson,
          "tail" -> view.history.lastBlockIds(view.history.bestBlock, count).map(id => Base58.encode(id).asJson).asJson
        ).asJson)
      }
    }
  }

}