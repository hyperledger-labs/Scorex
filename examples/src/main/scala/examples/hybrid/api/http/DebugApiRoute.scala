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
import scorex.core.api.http.{ApiRouteWithFullView, ApiResponse}
import scorex.core.settings.RESTApiSettings
import scorex.crypto.encode.Base58

import scala.util.Try


case class DebugApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory)
  extends ApiRouteWithFullView[HybridHistory, HBoxStoredState, HWallet, SimpleBoxTransactionMemPool] {

  override val route: Route = (pathPrefix("debug") & withCors) {
    infoRoute ~ chain ~ delay ~ myblocks ~ generators
  }

  def delay: Route = {
    (get & path("delay" / Segment / IntNumber)) { case (encodedSignature, count) =>
      withNodeView { view =>
        val result: Try[String] = for {
          id <- Base58.decode(encodedSignature)
          delay <- view.history.averageDelay(ModifierId @@ id, count)
        } yield delay.toString
        ApiResponse("delay" -> result.getOrElse("Undefined"))
      }
    }
  }

  def infoRoute: Route = (get & path("info")) {
    withNodeView { view =>
      val bestBlockJson = view.history.bestBlock match {
        case block: PosBlock => block.asJson
        case _ => view.history.bestBlock.asInstanceOf[PowBlock].asJson
      }

      ApiResponse(
        "height" -> view.history.height.toString.asJson,
        "bestPoS" -> Base58.encode(view.history.bestPosId).asJson,
        "bestPoW" -> Base58.encode(view.history.bestPowId).asJson,
        "bestBlock" -> bestBlockJson,
        "stateVersion" -> Base58.encode(view.state.version).asJson
      )
    }
  }

  def myblocks: Route = (get & path("myblocks")) {
    withNodeView { view =>
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

      ApiResponse(
        "pubkeys" -> pubkeys.map(pk => Base58.encode(pk.pubKeyBytes)).asJson,
        "count" -> (posCount + powCount).asJson,
        "posCount" -> posCount.asJson,
        "powCount" -> powCount.asJson
      )
    }
  }

  def generators: Route = (get & path("generators")) {
    withNodeView { view =>
      val map: Map[String, Int] = view.history.generatorDistribution()
        .map(d => Base58.encode(d._1.pubKeyBytes) -> d._2)
      ApiResponse(map.asJson)
    }
  }

  def chain: Route = (get & path("chain")) {
    withNodeView { view =>
      ApiResponse("history" -> view.history.toString)
    }
  }
}
