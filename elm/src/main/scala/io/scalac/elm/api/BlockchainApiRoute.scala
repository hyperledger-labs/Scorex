package io.scalac.elm.api

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.{ContentTypes, MediaTypes}
import akka.http.scaladsl.server._
import akka.pattern.ask
import io.circe._
import io.circe.syntax._
import io.scalac.elm.consensus.ElmBlockchain
import io.scalac.elm.util.ByteKey
import io.swagger.annotations._
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.api.http.ApiRoute
import scorex.core.settings.Settings

@Path("/blockchain")
@Api(value = "/blockchain")
class BlockchainApiRoute(val settings: Settings, nodeViewHolder: ActorRef)(implicit val context: ActorRefFactory) extends ApiRoute {

  import context.dispatcher

  implicit val jsonMarshaller: ToEntityMarshaller[Json] =
    Marshaller.StringMarshaller.wrap(MediaTypes.`application/json`)(_.spaces4)

  override lazy val route: Route = pathPrefix("blockchain") {
    blocks
  }

  @Path("/blocks")
  @ApiOperation(value = "get blocks' IDs", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "list of block IDs ordered as in the blockchain")
  ))
  def blocks = get {
    path("blocks") {
      complete {
        getBlockchain.map(_.blockIds.toList.sortBy(_._1).map(_._2.key.base58).asJson)
      }
    }
  }

  @Path("/block/{id}")
  @ApiOperation(value = "get block by ID", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "id", required = true, dataType = "string", paramType = "path", value = "XxYyZz")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "JSON representation of a block"),
    new ApiResponse(code = 404, message = "block not found")
  ))
  def block = get {
    path("block" / Segment) { id =>
      rejectEmptyResponse {
        complete {
          getBlockchain.map(_.blockById(ByteKey.base58(id).array).map(_.json))
        }
      }
    }
  }

  def getBlockchain =
    nodeViewHolder.ask(NodeViewHolder.GetCurrentView).mapTo[CurrentView[ElmBlockchain, _, _, _]].map(_.history)

}
