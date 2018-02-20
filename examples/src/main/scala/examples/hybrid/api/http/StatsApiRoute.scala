package examples.hybrid.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import examples.commons.SimpleBoxTransactionMemPool
import examples.hybrid.history.HybridHistory
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import io.circe.Json
import io.circe.syntax._
import scorex.core.ModifierId
import scorex.core.api.http.{ApiException, ApiRouteWithFullView, ApiTry, SuccessApiResponse}
import scorex.core.settings.RESTApiSettings
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

case class StatsApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory)
  extends ApiRouteWithFullView[HybridHistory, HBoxStoredState, HWallet, SimpleBoxTransactionMemPool] {

  override val route = pathPrefix("stats") {
    tail ~ meanDifficulty
  }

  def tail: Route = path("tail" / IntNumber) { count =>
    val tail = viewAsync().map { view =>
      SuccessApiResponse(Map(
        "count" -> count.asJson,
        "tail" -> view.history.lastBlockIds(view.history.bestBlock, count).map(id => Base58.encode(id).asJson).asJson
      ).asJson)
    }
    onComplete(tail) {
      case Success(r) => jsonRoute(r, get)
      case Failure(ex) => jsonRoute(ApiException(ex), get)
    }
  }

  def meanDifficulty: Route = path("meanDifficulty" / IntNumber / IntNumber) { (start, end) =>
    val meanDifficulty = viewAsync().map { view =>
      ApiTry {
        val count = (view.history.height - start).toInt
        val ids: Seq[ModifierId] = view.history.lastBlockIds(view.history.bestBlock, count).take(end - start)
        val posDiff = ids.flatMap(id => Try(view.history.storage.getPoSDifficulty(id)).toOption)
        val powDiff = ids.flatMap(id => Try(view.history.storage.getPoWDifficulty(Some(id))).toOption)
        val json: Json = Map(
          "posDiff" -> (posDiff.sum / posDiff.length).asJson,
          "powDiff" -> (powDiff.sum / powDiff.length).asJson
        ).asJson
        json
      }
    }
    onComplete(meanDifficulty) {
      case Success(r) => jsonRoute(r, get)
      case Failure(ex) => jsonRoute(ApiException(ex), get)
    }
  }

}