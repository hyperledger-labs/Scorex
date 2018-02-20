package examples.hybrid.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import examples.commons.SimpleBoxTransactionMemPool
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import examples.hybrid.history.HybridHistory
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import io.circe.syntax._
import scorex.core.ModifierId
import scorex.core.api.http.{ApiException, ApiRouteWithFullView, SuccessApiResponse}
import scorex.core.settings.RESTApiSettings
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


case class DebugApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory)
  extends ApiRouteWithFullView[HybridHistory, HBoxStoredState, HWallet, SimpleBoxTransactionMemPool] {

  override val route = pathPrefix("debug") {
    infoRoute ~ chain ~ delay ~ myblocks ~ generators
  }

  def delay: Route = {
    path("delay" / Segment / IntNumber) { case (encodedSignature, count) =>
      val delay = viewAsync().map { view =>
        SuccessApiResponse(Map(
          "delay" -> Base58.decode(encodedSignature).flatMap(id => view.history.averageDelay(ModifierId @@ id, count))
            .map(_.toString).getOrElse("Undefined")
        ).asJson)
      }
      onComplete(delay) {
        case Success(r) => jsonRoute(r, get)
        case Failure(ex) => jsonRoute(ApiException(ex), get)
      }
    }
  }

  def infoRoute: Route = path("info") {
    val info = viewAsync().map { view =>
      SuccessApiResponse(Map(
        "height" -> view.history.height.toString.asJson,
        "bestPoS" -> Base58.encode(view.history.bestPosId).asJson,
        "bestPoW" -> Base58.encode(view.history.bestPowId).asJson,
        "bestBlock" -> view.history.bestBlock.json,
        "stateVersion" -> Base58.encode(view.state.version).asJson
      ).asJson)
    }
    onComplete(info) {
      case Success(r) => jsonRoute(r, get)
      case Failure(ex) => jsonRoute(ApiException(ex), get)
    }
  }

  def myblocks: Route = path("myblocks") {
    val myBlocks = viewAsync().map { view =>
      val pubkeys = view.vault.publicKeys

      def isMyPosBlock(b: HybridBlock): Boolean = b match {
        case pos: PosBlock => pubkeys.exists(_.pubKeyBytes sameElements pos.generatorBox.proposition.pubKeyBytes)
        case _ => false
      }

      def isMyPowBlock(b: HybridBlock): Boolean = b match {
        case pow: PowBlock => pubkeys.exists(_.pubKeyBytes sameElements pow.generatorProposition.pubKeyBytes)
        case _ => false
      }

      val posCount = view.history.count(isMyPosBlock)
      val powCount = view.history.count(isMyPowBlock)

      SuccessApiResponse(Map(
        "pubkeys" -> pubkeys.map(pk => Base58.encode(pk.pubKeyBytes)).asJson,
        "count" -> (posCount + powCount).asJson,
        "posCount" -> posCount.asJson,
        "powCount" -> powCount.asJson
      ).asJson)
    }
    onComplete(myBlocks) {
      case Success(r) => jsonRoute(r, get)
      case Failure(ex) => jsonRoute(ApiException(ex), get)
    }
  }

  def generators: Route = path("generators") {
    val generators = viewAsync().map { view =>
      val map: Map[String, Int] = view.history.generatorDistribution()
        .map(d => Base58.encode(d._1.pubKeyBytes) -> d._2)
      SuccessApiResponse(map.asJson)
    }
    onComplete(generators) {
      case Success(r) => jsonRoute(r, get)
      case Failure(ex) => jsonRoute(ApiException(ex), get)
    }
  }

  def chain: Route = path("chain") {
    val chain = viewAsync().map { view =>
      SuccessApiResponse(Map(
        "history" -> view.history.toString
      ).asJson)
    }
    onComplete(chain) {
      case Success(r) => jsonRoute(r, get)
      case Failure(ex) => jsonRoute(ApiException(ex), get)
    }
  }
}
