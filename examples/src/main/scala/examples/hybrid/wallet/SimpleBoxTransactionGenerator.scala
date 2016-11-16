package examples.hybrid.wallet

import akka.actor.{Actor, ActorRef}
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.crypto.sign.PrivateKey25519
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

  def generate(wallet: HWallet): Try[SimpleBoxTransaction] = Try {
    val from: IndexedSeq[(PrivateKey25519, Long, Long)] = wallet.boxes()
      .filter(p => Random.nextBoolean())
      .take(wallet.publicKeys.size / 2 - 1)
      .map { b =>
      (wallet.secretByPublicImage(b.box.proposition).get, b.box.nonce, b.box.value)
    }.toIndexedSeq
    var canSend = from.map(_._3).sum

    val to: IndexedSeq[(PublicKey25519Proposition, Long)] = wallet.publicKeys
      .filter(p => Random.nextBoolean()).map { p =>
      val amount = Random.nextInt(canSend.toInt - 1).toLong % canSend
      canSend = canSend - amount
      (p, amount)
    }.toIndexedSeq

    val fee: Long = from.map(_._3).sum - to.map(_._2).sum
    val timestamp = System.currentTimeMillis()
    SimpleBoxTransaction(from.map(t=> t._1 -> t._2), to, fee, timestamp)
  }
}

object SimpleBoxTransactionGenerator {
  case class StartGeneration(delay: FiniteDuration)
}
