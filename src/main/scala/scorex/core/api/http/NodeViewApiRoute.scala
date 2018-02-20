package scorex.core.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.consensus.History
import scorex.core.network.ConnectedPeer
import scorex.core.settings.RESTApiSettings
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Vault
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.{ModifierId, PersistentNodeViewModifier}
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}


case class NodeViewApiRoute[P <: Proposition, TX <: Transaction[P]]
(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
(implicit val context: ActorRefFactory) extends ApiRoute {

  import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView

  override val route = pathPrefix("nodeView") {
    openSurface ~ persistentModifierById ~ pool
  }

  type PM <: PersistentNodeViewModifier
  type HIS <: History[PM, _, _ <: History[PM, _, _]]
  type MP <: MemoryPool[TX, _ <: MemoryPool[TX, _]]
  type MS <: MinimalState[PM, _ <: MinimalState[_, _]]
  type VL <: Vault[P, TX, PM, _ <: Vault[P, TX, PM, _]]

  //TODO null?
  private val source: ConnectedPeer = null

  case class OpenSurface(ids: Seq[ModifierId])

  def getOpenSurface: Future[OpenSurface] = {
    def f(v: CurrentView[HIS, MS, VL, MP]): OpenSurface = OpenSurface(v.history.openSurfaceIds())

    (nodeViewHolderRef ? GetDataFromCurrentView(f)).map(_.asInstanceOf[OpenSurface])
  }

  case class MempoolData(size: Int, transactions: Iterable[TX])

  def getMempool: Future[MempoolData] = {
    def f(v: CurrentView[HIS, MS, VL, MP]): MempoolData = MempoolData(v.pool.size, v.pool.take(1000))

    (nodeViewHolderRef ? GetDataFromCurrentView(f)).map(_.asInstanceOf[MempoolData])
  }

  def pool: Route = path("pool") {
    onComplete(getMempool) {
      case Success(mpd) => jsonRoute(SuccessApiResponse(
        Map(
          "size" -> mpd.size.asJson,
          "transactions" -> mpd.transactions.map(_.json).asJson
        ).asJson
      ), get)
      case Failure(e) => jsonRoute(ApiException(e), get)
    }
  }

  def openSurface: Route = path("openSurface") {
    onComplete(getOpenSurface) {
      case Success(os) => jsonRoute(SuccessApiResponse(os.ids.map(Base58.encode).asJson), get)
      case Failure(ex)  => jsonRoute(ApiException(ex), get)
    }
  }

  def persistentModifierById: Route = path("persistentModifier" / Segment) { encodedId =>
    val persistentModifier = Base58.decode(encodedId) match {
      case Success(rawId) =>
        val id = ModifierId @@ rawId

        def f(v: CurrentView[HIS, MS, VL, MP]): Option[PM] = v.history.modifierById(id)

        (nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, Option[PM]](f)).mapTo[Option[PM]]
          .map(_.map(tx => SuccessApiResponse(tx.json)).getOrElse(ApiError.notExists))
      case _ => Future(ApiError.notExists)
    }
    onComplete(persistentModifier) {
      case Success(r) => jsonRoute(r, get)
      case Failure(ex) => jsonRoute(ApiException(ex), get)
    }
  }

}
