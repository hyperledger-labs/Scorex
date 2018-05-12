package examples.hybrid.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.commons.Value
import examples.hybrid.history.HybridHistory
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import io.circe.parser._
import io.circe.syntax._
import scorex.core.api.http.{ApiResponse, ApiRouteWithFullView}
import scorex.core.settings.RESTApiSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.PublicKey

import scala.util.{Failure, Success, Try}


case class WalletApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                         (implicit val context: ActorRefFactory)
  extends ApiRouteWithFullView[HybridHistory, HBoxStoredState, HWallet, SimpleBoxTransactionMemPool] {

  import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction

  //TODO move to settings?
  val DefaultFee: Int = 100

  override val route: Route = (pathPrefix("wallet") & withCors) {
    balances ~ transfer
  }

  def transfer: Route = (get & path("transfer")) {
    entity(as[String]) { body =>
      withAuth {
        withNodeView { view =>
          parse(body) match {
            case Left(failure) => complete(ApiResponse(failure.getCause))
            case Right(json) => Try {
              val wallet = view.vault
              // TODO: Can we do this extraction in a safer way (not calling head/get)?
              @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
              val amount: Long = (json \\ "amount").head.asNumber.get.toLong.get
              // TODO: Can we do this extraction in a safer way (not calling head/get)?
              @SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
              val recipient: PublicKey25519Proposition = PublicKey25519Proposition(PublicKey @@ Base58.decode((json \\ "recipient").head.asString.get).get)
              val fee: Long = (json \\ "fee").headOption.flatMap(_.asNumber).flatMap(_.toLong).getOrElse(DefaultFee)
              val tx = SimpleBoxTransaction.create(wallet, Seq((recipient, Value @@ amount)), fee).get
              nodeViewHolderRef ! LocallyGeneratedTransaction[PublicKey25519Proposition, SimpleBoxTransaction](tx)
              tx.asJson
            } match {
              case Success(resp) => complete(ApiResponse(resp))
              case Failure(e) => complete(ApiResponse(e))
            }
          }
        }
      }
    }
  }

  def balances: Route = (get & path("balances")) {
    withNodeView { view =>
      val wallet = view.vault
      val boxes = wallet.boxes()
      complete(ApiResponse(
        "totalBalance" -> boxes.map(_.box.value.toLong).sum.toString.asJson,
        "publicKeys" -> wallet.publicKeys.map(pk => Base58.encode(pk.pubKeyBytes)).asJson,
        "boxes" -> boxes.map(_.box.asJson).asJson
      ))
    }
  }

}
