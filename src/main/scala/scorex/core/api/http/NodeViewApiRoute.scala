package scorex.core.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.consensus.History
import scorex.core.serialization.SerializerRegistry
import scorex.core.settings.RESTApiSettings
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Vault
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.utils.ScorexEncoding
import scorex.core.PersistentNodeViewModifier
import scorex.util.{ModifierId, bytesToId}

import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag
import scala.util.{Failure, Success}


case class NodeViewApiRoute[TX <: Transaction]
(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
(implicit val context: ActorRefFactory, val serializerReg: SerializerRegistry, val ec: ExecutionContext)
  extends ApiRoute with ScorexEncoding {

  import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView

  override val route: Route = pathPrefix("nodeView") {
    corsHandler(
      openSurface ~ persistentModifierById ~ pool
    )
  }

  type PM <: PersistentNodeViewModifier
  type HIS <: History[PM, _, _ <: History[PM, _, _]]
  type MP <: MemoryPool[TX, _ <: MemoryPool[TX, _]]
  type MS <: MinimalState[PM, _ <: MinimalState[_, _]]
  type VL <: Vault[TX, PM, _ <: Vault[TX, PM, _]]

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
    encoder.decode(encodedId) match {
      case Failure(_) => ApiError.NotExists
      case Success(rawId) =>
        val id = bytesToId(rawId)

        def f(v: CurrentView[HIS, MS, VL, MP]): Option[PM] = v.history.modifierById(id)

        val futurePersistentModifier = (nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, Option[PM]](f)).mapTo[Option[PM]]
        onComplete(futurePersistentModifier) {
          case Success(Some(tx)) => fn(tx)
          case Success(None) => ApiError.NotExists
          case Failure(_) => ApiError.NotExists
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
        ApiError(serializationErrors.map(_.left.get))
      else
        ApiResponse(
          "size" -> mpd.size.asJson,
          "transactions" -> txAsJson.map(_.right.get).asJson
        )
    }
  }

  def openSurface: Route = (get & path("openSurface")) {
    withOpenSurface { os =>
      ApiResponse(os.ids.map(encoder.encodeId).asJson)
    }
  }

  def persistentModifierById: Route = (get & path("persistentModifier" / Segment)) { encodedId =>
    withPersistentModifier(encodedId) { tx =>
      val clazz = ClassTag(tx.getClass).runtimeClass
      ApiResponse(serializerReg.toJson(clazz, tx))
    }
  }

}
