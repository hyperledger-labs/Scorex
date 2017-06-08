package examples.hybrid.wallet

import akka.actor.{Actor, ActorRef}
import examples.commons.SimpleBoxTransaction
import examples.hybrid.state.HBoxStoredState
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Random, Success, Try}

/**
  * Generator of SimpleBoxTransaction inside a wallet
  */
class SimpleBoxTransactionGenerator(viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  import SimpleBoxTransactionGenerator._

  override def receive: Receive = {
    case StartGeneration(duration) =>
      context.system.scheduler.schedule(duration, duration, viewHolderRef, GetCurrentView)

    case CurrentView(_, state: HBoxStoredState, wallet: HWallet, _) =>
      generate(wallet) match {
        case Success(tx) =>
          log.info(s"Local tx with with ${tx.from.size} inputs, ${tx.to.size} outputs. Valid: ${tx.semanticValidity}")
          viewHolderRef ! LocallyGeneratedTransaction[PublicKey25519Proposition, SimpleBoxTransaction](tx)
        case Failure(e) =>
          e.printStackTrace()
      }
  }

  private val ex: ArrayBuffer[Array[Byte]] = ArrayBuffer()

  def generate(wallet: HWallet): Try[SimpleBoxTransaction] = {
    if (Random.nextInt(100) == 1) ex.clear()

    val pubkeys = wallet.publicKeys.toSeq
    if (pubkeys.size < 10) wallet.generateNewSecret()
    val recipients = scala.util.Random.shuffle(pubkeys).take(Random.nextInt(pubkeys.size))
      .map(r => (r, Random.nextInt(100).toLong))
    val tx = SimpleBoxTransaction.create(wallet, recipients, Random.nextInt(100), ex)
    tx.map(t => t.boxIdsToOpen.foreach(id => ex += id))
    tx
  }
}

object SimpleBoxTransactionGenerator {

  case class StartGeneration(delay: FiniteDuration)

}
