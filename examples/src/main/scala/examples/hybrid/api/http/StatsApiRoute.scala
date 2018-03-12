package examples.hybrid.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import examples.commons.SimpleBoxTransactionMemPool
import examples.hybrid.history.HybridHistory
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import io.circe.syntax._
import scorex.core.ModifierId
import scorex.core.api.http.{ApiRouteWithFullView, ApiTry, SuccessApiResponse}
import scorex.core.settings.RESTApiSettings
import scorex.crypto.encode.Base58

import scala.util.Try

case class StatsApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory)
  extends ApiRouteWithFullView[HybridHistory, HBoxStoredState, HWallet, SimpleBoxTransactionMemPool] {

  override val route = (pathPrefix("stats") & withCors) {
    tail ~ meanDifficulty
  }

  def tail: Route = (get & path("tail" / IntNumber)) { count =>
    withNodeView { view =>
      val lastBlockIds = view.history.lastBlockIds(view.history.bestBlock, count)
      val tail = lastBlockIds.map(id => Base58.encode(id).asJson)
      complete(SuccessApiResponse("count" -> count.asJson, "tail" -> tail.asJson))
    }
  }

  def meanDifficulty: Route = (get & path("meanDifficulty" / IntNumber / IntNumber)) { (start, end) =>
    withNodeView { view =>
      ApiTry {
        val count = (view.history.height - start).toInt
        val ids: Seq[ModifierId] = view.history.lastBlockIds(view.history.bestBlock, count).take(end - start)
        val posDiff = ids.flatMap(id => Try(view.history.storage.getPoSDifficulty(id)).toOption)
        val powDiff = ids.flatMap(id => Try(view.history.storage.getPoWDifficulty(Some(id))).toOption)
        complete(SuccessApiResponse(
          "posDiff" -> (posDiff.sum / posDiff.length).asJson,
          "powDiff" -> (powDiff.sum / powDiff.length).asJson
        ))
      }
    }
  }

}