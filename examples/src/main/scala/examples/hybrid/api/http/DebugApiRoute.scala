package examples.hybrid.api.http

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import examples.hybrid.history.HybridHistory
import examples.hybrid.mempool.HMemPool
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import io.circe.syntax._
import io.swagger.annotations._
import play.api.libs.json.Json
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
    infoRoute ~ chain ~ delay
  }

  @Path("/delay/{id}/{blockNum}")
  @ApiOperation(value = "Average delay",
    notes = "Average delay in milliseconds between last $blockNum blocks starting from block with $id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "id", value = "Base58-encoded id", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "blockNum", value = "Number of blocks to count delay", required = true, dataType = "string", paramType = "path")
  ))
  def delay: Route = {
    path("delay" / Segment / IntNumber) { case (encodedSignature, count) =>
      getJsonRoute {
        getView().map { view =>
          Map(
            "delay" -> Base58.decode(encodedSignature).flatMap(id => view.history.averageDelay(id, count))
              .map(_.toString).getOrElse("Undefined")
          ).asJson
        }
      }
    }
  }

  @Path("/info")
  @ApiOperation(value = "Info", notes = "Debug info about blockchain", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def infoRoute: Route = path("info") {
    getJsonRoute {
      getView().map { view =>
        Map(
          "height" -> view.history.height.toString.asJson,
          "bestPoS" -> Base58.encode(view.history.bestPosId).asJson,
          "bestPoW" -> Base58.encode(view.history.bestPowId).asJson,
          "stateVersion" -> Base58.encode(view.state.version).asJson
        ).asJson
      }
    }
  }

  @Path("/chain")
  @ApiOperation(value = "Chain", notes = "Print full chain", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def chain: Route = path("chain") {
    getJsonRoute {
      getView().map { view =>
        Map(
          "history" -> view.history.toString
        ).asJson
      }
    }
  }
}