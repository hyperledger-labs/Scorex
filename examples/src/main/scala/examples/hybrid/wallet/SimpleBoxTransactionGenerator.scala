package examples.hybrid.wallet

import akka.actor.{Actor, ActorRef}
import examples.commons.SimpleBoxTransaction
import examples.hybrid.state.HBoxStoredState
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Random, Success, Try}

/**
  * Generator of SimpleBoxTransaction inside a wallet
  */
class SimpleBoxTransactionGenerator(viewHolderRef: ActorRef) extends Actor {

  import SimpleBoxTransactionGenerator._

  override def receive: Receive = {
    case StartGeneration(duration) =>
      context.system.scheduler.schedule(duration, duration, viewHolderRef, GetCurrentView)

    case CurrentView(_, state: HBoxStoredState, wallet: HWallet, _) =>
      generate(wallet) match {
        case Success(tx) =>
          println(s"Local tx with with ${tx.from.size} inputs, ${tx.to.size} outputs")
          viewHolderRef ! LocallyGeneratedTransaction[PublicKey25519Proposition, SimpleBoxTransaction](tx)
        case Failure(e) =>
          e.printStackTrace()
      }
  }

  def generate(wallet: HWallet): Try[SimpleBoxTransaction] = {
    val pubkeys = wallet.publicKeys.toSeq
    //todo multiple recipients
    val recipient = pubkeys(Random.nextInt(pubkeys.size))
    SimpleBoxTransaction.create(wallet, recipient, Random.nextInt(100), Random.nextInt(100))
  }
}

object SimpleBoxTransactionGenerator {

  case class StartGeneration(delay: FiniteDuration)

}
