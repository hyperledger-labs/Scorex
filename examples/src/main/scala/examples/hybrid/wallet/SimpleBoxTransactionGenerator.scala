package examples.hybrid.wallet

import akka.actor.{Actor, ActorRef}
import examples.hybrid.state.SimpleBoxTransaction
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion

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

    case CurrentView(_, _, wallet: HWallet, _) =>
      if (wallet.secrets.size < 5) wallet.generateNewSecret()

      generate(wallet) match {
        case Success(tx) =>
          viewHolderRef ! LocallyGeneratedTransaction[PublicKey25519Proposition, SimpleBoxTransaction](tx)
        case Failure(e) =>
          e.printStackTrace()
      }
  }

  def generate(wallet: HWallet): Try[SimpleBoxTransaction] = Try {
    val from: IndexedSeq[(PublicKey25519Proposition, Long)] = wallet.boxes()
      .filter(p => Random.nextBoolean()).map { b =>
      (b.box.proposition, b.box.value)
    }.toIndexedSeq
    var canSend = from.map(_._2).sum

    val to: IndexedSeq[(PublicKey25519Proposition, Long)] = wallet.publicKeys
      .filter(p => Random.nextBoolean()).map { p =>
      val amount = Random.nextLong() % canSend
      canSend = canSend - amount
      (p, amount)
    }.toIndexedSeq

    val fee: Long = from.map(_._2).sum - to.map(_._2).sum
    val timestamp = System.currentTimeMillis()
    val unsigned = SimpleBoxTransaction(from, to, IndexedSeq(), fee, timestamp)

    val message: Array[Byte] = unsigned.messageToSign

    val signatures: IndexedSeq[Signature25519] = from.map { p =>
      val sec = wallet.secretByPublicImage(p._1).get
      PrivateKey25519Companion.sign(sec, message)
    }
    unsigned.copy(signatures = signatures).ensuring(_.valid)
  }

}

object SimpleBoxTransactionGenerator {

  case class StartGeneration(delay: FiniteDuration)

}
