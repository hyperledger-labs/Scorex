package examples.hybrid.api.http

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.settings.Settings

import scala.concurrent.ExecutionContext.Implicits.global


@Path("/wallet")
@Api(value = "/wallet", produces = "application/json")
case class WalletApiRoute(override val settings: Settings, nodeViewHolderRef: ActorRef)
                         (implicit val context: ActorRefFactory) extends ApiRouteWithView {

  override val route = pathPrefix("wallet") {
    balances
  }

  @Path("/balances")
  @ApiOperation(value = "Balances", notes = "Return info about local wallet", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def balances: Route = path("balances") {
    getJsonRoute {
      getView().map { view =>
        val wallet = view.vault
        val boxes = wallet.boxes()

        Map(
          "totalBalance" -> boxes.map(_.box.value).sum.toString.asJson,
          "boxes" -> boxes.map(_.box.json).asJson
        ).asJson
      }
    }
  }

}