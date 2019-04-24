package examples.hybrid.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool, Value}
import examples.hybrid.history.HybridHistory
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HBoxWallet
import io.circe.parser._
import io.circe.syntax._
import scorex.core.api.http.{ApiError, ApiResponse, ApiRouteWithFullView}
import scorex.core.settings.RESTApiSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexEncoding
import scorex.crypto.signatures.PublicKey

import scala.util.{Failure, Success, Try}


case class WalletApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                         (implicit val context: ActorRefFactory)
  extends ApiRouteWithFullView[HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionMemPool]
    with ScorexEncoding {

  import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction

  //TODO move to settings?
  val DefaultFee: Int = 100

  override val route: Route = (pathPrefix("wallet")) {
    corsHandler(
      balances ~ transfer
    )
  }

  def transfer: Route = (get & path("transfer")) {
    entity(as[String]) { body =>
      withAuth {
        withNodeView { view =>
          parse(body) match {
            case Left(failure) => ApiError(failure.getCause)
            case Right(json) => Try {
              val wallet = view.vault
              // TODO: Can we do this extraction in a safer way (not calling head/get)?
              @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
              val amount: Long = (json \\ "amount").head.asNumber.get.toLong.get
              // TODO: Can we do this extraction in a safer way (not calling head/get)?
              @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
              val recipient: PublicKey25519Proposition = PublicKey25519Proposition(PublicKey @@ encoder.decode((json \\ "recipient").head.asString.get).get)
              val fee: Long = (json \\ "fee").headOption.flatMap(_.asNumber).flatMap(_.toLong).getOrElse(DefaultFee)
              val tx = SimpleBoxTransaction.create(wallet, Seq((recipient, Value @@ amount)), fee).get
              nodeViewHolderRef ! LocallyGeneratedTransaction[SimpleBoxTransaction](tx)
              tx.asJson
            } match {
              case Success(resp) => ApiResponse(resp)
              case Failure(e) => ApiError(e)
            }
          }
        }
      }
    }
  }

  def balances: Route = (get & path("balances")) {
    withNodeView { view =>
      val wallet = view.vault
      val boxes = wallet.boxes().map(_.box)
      ApiResponse(
        "totalBalance" -> boxes.map(_.value.toLong).sum.toString.asJson,
        "publicKeys" -> wallet.publicKeys.map(pk => encoder.encode(pk.pubKeyBytes)).asJson,
        "boxes" -> boxes.asJson
      )
    }
  }

}
