package scorex.core.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.util.ByteString
import io.circe.syntax._
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.consensus.History
import scorex.core.network.ConnectedPeer
import scorex.core.serialization.SerializerRegistry
import scorex.core.settings.RESTApiSettings
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Vault
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.{ModifierId, PersistentNodeViewModifier}
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}


case class NodeViewApiRoute[P <: Proposition, TX <: Transaction[P]]
(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
(implicit val context: ActorRefFactory, val serializerReg: SerializerRegistry) extends ApiRoute {

  import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView

  override val route = (pathPrefix("nodeView") & withCors) {
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

  def withOpenSurface(fn: OpenSurface => Route): Route = {
    def f(v: CurrentView[HIS, MS, VL, MP]): OpenSurface = OpenSurface(v.history.openSurfaceIds())
    val futureOpenSurface = (nodeViewHolderRef ? GetDataFromCurrentView(f)).map(_.asInstanceOf[OpenSurface])
    onSuccess(futureOpenSurface)(fn)
  }

  case class MempoolData(size: Int, transactions: Iterable[TX])

  def withMempool(fn: MempoolData => Route): Route = {
    def f(v: CurrentView[HIS, MS, VL, MP]): MempoolData = MempoolData(v.pool.size, v.pool.take(1000))
    val futureMempoolData = (nodeViewHolderRef ? GetDataFromCurrentView(f)).map(_.asInstanceOf[MempoolData])
    onSuccess(futureMempoolData)(fn)
  }

  def withPersistentModifier(encodedId: String)(fn: PM => Route): Route = {
    Base58.decode(encodedId) match {
      case Failure(e) => complete(ApiError.notExists)
      case Success(rawId) =>
        val id: ModifierId = ModifierId @@ rawId.toSeq

        def f(v: CurrentView[HIS, MS, VL, MP]): Option[PM] = v.history.modifierById(id)

        val futurePersistentModifier = (nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, Option[PM]](f)).mapTo[Option[PM]]
        onComplete(futurePersistentModifier) {
          case Success(Some(tx)) => fn(tx)
          case Success(None) => complete(ApiError.notExists)
          case Failure(_) => complete(ApiError.notExists)
        }
    }
  }

  def pool: Route = (get & path("pool")) {
    withMempool { mpd =>

      val txAsJson = mpd.transactions.map(t => {
        val clazz = ClassTag(t.getClass).runtimeClass
        serializerReg.toJson(clazz, t)
      })

      val serializationErrors = txAsJson.filter(_.isLeft).toList
      if (serializationErrors.nonEmpty)
        complete(ApiError(serializationErrors.map(_.left.get.getMessage).mkString(","), StatusCodes.InternalServerError))
      else
        complete(SuccessApiResponse(
          "size" -> mpd.size.asJson,
          "transactions" -> txAsJson.map(_.right.get).asJson
        ))
    }
  }

  def openSurface: Route = (get & path("openSurface")) {
    withOpenSurface { os =>
      complete(SuccessApiResponse(os.ids.map(id => Base58.encode(id.toArray)).asJson))
    }
  }

  def persistentModifierById: Route = (get & path("persistentModifier" / Segment)) { encodedId =>
    withPersistentModifier(encodedId) { tx =>
      val clazz = ClassTag(tx.getClass).runtimeClass
      val jsonOrErr = serializerReg.toJson(clazz, tx)
      val response: ScorexApiResponse = Try(SuccessApiResponse(jsonOrErr.right.get))
        .getOrElse(ApiError(jsonOrErr.left.get.getMessage, StatusCodes.InternalServerError))
      complete(response)
    }
  }

}
