package examples.hybrid.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import examples.commons.SimpleBoxTransactionMemPool
import examples.hybrid.history.HybridHistory
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HBoxWallet
import io.circe.syntax._
import scorex.core.api.http.{ApiResponse, ApiRouteWithFullView, ApiTry}
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.{ScorexEncoding, ScorexLogging}
import scorex.util.ModifierId

import scala.util.Try

case class StatsApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory)
  extends ApiRouteWithFullView[HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionMemPool]
    with ScorexEncoding {

  override val route: Route = (pathPrefix("stats") & withCors) {
    tail ~ meanDifficulty
  }

  def tail: Route = (get & path("tail" / IntNumber)) { count =>
    withNodeView { view =>
      val lastBlockIds = view.history.lastBlockIds(view.history.bestBlock, count)
      val tail = lastBlockIds.map(id => encoder.encode(id).asJson)
      ApiResponse("count" -> count.asJson, "tail" -> tail.asJson)
    }
  }

  def meanDifficulty: Route = (get & path("meanDifficulty" / IntNumber / IntNumber)) { (start, end) =>
    withNodeView { view =>
      ApiTry {
        val count = (view.history.height - start).toInt
        val ids: Seq[ModifierId] = view.history.lastBlockIds(view.history.bestBlock, count).take(end - start)
        val posDiff = ids.flatMap(id => Try(view.history.storage.getPoSDifficulty(id)).toOption)
        val powDiff = ids.flatMap(id => Try(view.history.storage.getPoWDifficulty(Some(id))).toOption)
        ApiResponse(
          "posDiff" -> (posDiff.sum / posDiff.length).asJson,
          "powDiff" -> (powDiff.sum / powDiff.length).asJson
        )
      }
    }
  }

}
