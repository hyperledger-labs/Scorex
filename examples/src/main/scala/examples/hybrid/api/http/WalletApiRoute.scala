package examples.hybrid.api.http

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import examples.hybrid.state.SimpleBoxTransaction
import io.circe.parser._
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.api.http.{ApiException, SuccessApiResponse}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}


@Path("/wallet")
@Api(value = "/wallet", produces = "application/json")
case class WalletApiRoute(override val settings: Settings, nodeViewHolderRef: ActorRef)
                         (implicit val context: ActorRefFactory) extends ApiRouteWithView {

  //TODO move to settings?
  val DefaultFee = 100

  override val route = pathPrefix("wallet") {
    balances ~ transfer
  }

  @Path("/transfer")
  @ApiOperation(value = "Transfer",
    notes = "Transfer coins from one output to another",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      defaultValue = "{\"recipient\":\"3FAskwxrbqiX2KGEnFPuD3z89aubJvvdxZTKHCrMFjxQ\",\"amount\":1,\"fee\":100}"
    )
  ))
  def transfer: Route = path("transfer") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          viewAsync().map { view =>
            parse(body) match {
              case Left(failure) => ApiException(failure.getCause)
              case Right(json) => Try {
                val wallet = view.vault
                val amount: Long = (json \\ "amount").head.asNumber.get.toLong.get
                val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((json \\ "recipient").head.asString.get).get)
                val fee: Long = (json \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(DefaultFee)

                val from: IndexedSeq[(PrivateKey25519, Long, Long)] = wallet.boxes().flatMap { b =>
                  wallet.secretByPublicImage(b.box.proposition).map(s => (s, b.box.nonce, b.box.value))
                }.toIndexedSeq
                var canSend = from.map(_._3).sum
                val charge: (PublicKey25519Proposition, Long) = (wallet.publicKeys.head, canSend - amount - fee)

                val to: IndexedSeq[(PublicKey25519Proposition, Long)] = IndexedSeq(charge, (recipient, amount))

                require(from.map(_._3).sum - to.map(_._2).sum == fee)

                val timestamp = System.currentTimeMillis()
                val tx: SimpleBoxTransaction = SimpleBoxTransaction(from.map(t => t._1 -> t._2), to, fee, timestamp)
                nodeViewHolderRef ! LocallyGeneratedTransaction[PublicKey25519Proposition, SimpleBoxTransaction](tx)
                tx.json
              } match {
                case Success(resp) => SuccessApiResponse(resp)
                case Failure(e) => ApiException(e)
              }
            }
          }
        }
      }
    }
  }


  @Path("/balances")
  @ApiOperation(value = "Balances", notes = "Return info about local wallet", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def balances: Route = path("balances") {
    getJsonRoute {
      viewAsync().map { view =>
        val wallet = view.vault
        val boxes = wallet.boxes()

        SuccessApiResponse(Map(
          "totalBalance" -> boxes.map(_.box.value).sum.toString.asJson,
          "boxes" -> boxes.map(_.box.json).asJson
        ).asJson)
      }
    }
  }

}