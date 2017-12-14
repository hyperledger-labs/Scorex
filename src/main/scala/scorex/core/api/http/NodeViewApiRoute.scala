package scorex.core.api.http

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.NodeViewHolder.{CurrentView, GetDataFromCurrentView}
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


@Path("/nodeView")
@Api(value = "/nodeView", produces = "application/json")
case class NodeViewApiRoute[P <: Proposition, TX <: Transaction[P]]
(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
(implicit val context: ActorRefFactory) extends ApiRoute {

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

  def getOpenSurface(): Try[OpenSurface] = Try {
    def f(v: CurrentView[HIS, MS, VL, MP]): OpenSurface = OpenSurface(v.history.openSurfaceIds())

    Await.result(nodeViewHolderRef ? GetDataFromCurrentView(f), 5.seconds).asInstanceOf[OpenSurface]
  }

  case class MempoolData(size: Int, transactions: Iterable[TX])

  def getMempool(): Try[MempoolData] = Try {
    def f(v: CurrentView[HIS, MS, VL, MP]): MempoolData = MempoolData(v.pool.size, v.pool.take(1000))

    Await.result(nodeViewHolderRef ? GetDataFromCurrentView(f), 5.seconds).asInstanceOf[MempoolData]
  }

  @Path("/pool")
  @ApiOperation(value = "Pool", notes = "Pool of unconfirmed transactions", httpMethod = "GET")
  def pool: Route = path("pool") {
    getJsonRoute {
      getMempool() match {
        case Success(mpd: MempoolData) => SuccessApiResponse(
          Map(
            "size" -> mpd.size.asJson,
            "transactions" -> mpd.transactions.map(_.json).asJson
          ).asJson
        )
        case Failure(e) => ApiException(e)
      }
    }
  }

  @Path("/openSurface")
  @ApiOperation(value = "Ids of open surface", notes = "Ids of open surface in history", httpMethod = "GET")
  def openSurface: Route = path("openSurface") {
    getJsonRoute {
      getOpenSurface() match {
        case Success(os: OpenSurface) => SuccessApiResponse(os.ids.map(Base58.encode).asJson)
        case Failure(e) => ApiException(e)
      }
    }
  }


  @Path("/persistentModifier/{id}")
  @ApiOperation(value = "Persistent modifier by id", notes = "Persistent modifier by id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "id", value = "block id ", required = true, dataType = "string", paramType = "path")
  ))
  def persistentModifierById: Route = path("persistentModifier" / Segment) { encodedId =>
    getJsonRoute {
      Base58.decode(encodedId) match {
        case Success(rawId) =>
          val id = ModifierId @@ rawId

          def f(v: CurrentView[HIS, MS, VL, MP]): Option[PM] = v.history.modifierById(id)

          (nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, Option[PM]](f)).mapTo[Option[PM]]
            .map(_.map(tx => SuccessApiResponse(tx.json)).getOrElse(ApiError.blockNotExists))
        case _ => Future(ApiError.blockNotExists)
      }
    }
  }

  @Path("/transaction/{id}")
  @ApiOperation(value = "Transaction by id", notes = "Transaction by id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "id", value = "block id ", required = true, dataType = "string", paramType = "path")
  ))
  def transactionById: Route = path("transaction" / Segment) { encodedId =>
    getJsonRoute {
      Base58.decode(encodedId) match {
        case Success(rawId) =>
          val id = ModifierId @@ rawId

          def f(v: CurrentView[HIS, MS, VL, MP]): Option[TX] = {
            v.history.modifierById(id) match {
              case Some(tx: TX@unchecked) if tx.isInstanceOf[TX] => Some(tx)
              case None => v.pool.getById(id)
            }
          }

          (nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, Option[TX]](f)).mapTo[Option[TX]]
            .map(_.map(tx => SuccessApiResponse(tx.json)).getOrElse(ApiError.transactionNotExists))
        case _ => Future(ApiError.transactionNotExists)
      }
    }
  }
}
