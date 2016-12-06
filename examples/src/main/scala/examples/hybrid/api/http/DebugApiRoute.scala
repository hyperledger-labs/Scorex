package examples.hybrid.api.http

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import examples.hybrid.history.HybridHistory
import examples.hybrid.mempool.HMemPool
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.api.http.ApiRoute
import scorex.core.settings.Settings
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


@Path("/debug")
@Api(value = "/debug", description = "Useful functions", position = 3, produces = "application/json")
case class DebugApiRoute(override val settings: Settings, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory) extends ApiRoute {

  def getView(): Future[CurrentView[HybridHistory, HBoxStoredState, HWallet, HMemPool]] = {
    (nodeViewHolderRef ? GetCurrentView)
      .mapTo[CurrentView[HybridHistory, HBoxStoredState, HWallet, HMemPool]]
  }

  override val route = pathPrefix("debug") {
    debugRoute
  }

  @Path("/info")
  @ApiOperation(value = "Info", notes = "Debug info about blockchain", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def debugRoute: Route = path("info") {
    getJsonRoute {
      getView().map { view =>
        Map(
          "height" -> view.history.powHeight.asJson,
          "stateVersion" -> Base58.encode(view.state.version).asJson
        ).asJson
      }
    }
  }

}