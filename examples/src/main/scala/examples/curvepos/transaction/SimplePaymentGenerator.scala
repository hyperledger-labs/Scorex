package examples.curvepos.transaction

import akka.actor.ActorRef
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import akka.pattern.ask
import akka.util.Timeout
import examples.curvepos.SimpleBlockchain

import scala.concurrent.Await
import scala.util.{Random, Try}
import scala.concurrent.duration._

class SimplePaymentGenerator(viewHolderRef: ActorRef, localInterfaceRef: ActorRef) {

  def generatePayment(amount: Long, to: PublicKey25519Proposition): Try[SimplePayment] = Try {
    implicit val timeout = Timeout(5.seconds)

    val f = viewHolderRef ? NodeViewHolder.GetCurrentView
    val fv = f.mapTo[CurrentView[SimpleBlockchain, SimpleState, SimpleWallet, SimpleMemPool]]

    val view = Await.result(fv, 5.seconds)

    val wallet: SimpleWallet = view.vault
    val state: SimpleState = view.state

    require(wallet.currentBalance >= amount, "Not enough money")
    val from = wallet.publicKeys.head
    val fee = Random.nextInt(4) + 1

    val nonce = state.boxesOf(from).head.nonce + 1
    val time = System.currentTimeMillis()

    SimplePayment(from, to, amount, fee, nonce, time)
  }

  def generateAndSend(amount: Long, to: PublicKey25519Proposition): Unit =
    generatePayment(amount, to)
      .foreach(payment =>
        localInterfaceRef ! LocallyGeneratedTransaction[PublicKey25519Proposition, SimplePayment](payment)
      )
}
