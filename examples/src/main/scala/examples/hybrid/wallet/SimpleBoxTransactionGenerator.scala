package examples.hybrid.wallet

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool, Value}
import examples.hybrid.history.HybridHistory
import examples.hybrid.state.HBoxStoredState
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.utils.ScorexLogging

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Random, Success, Try}

/**
  * Generator of SimpleBoxTransaction inside a wallet
  */
class SimpleBoxTransactionGenerator(viewHolderRef: ActorRef)(implicit ec: ExecutionContext) extends Actor
  with ScorexLogging {

  import SimpleBoxTransactionGenerator.ReceivableMessages._
  import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}

  private val getRequiredData: GetDataFromCurrentView[HybridHistory,
    HBoxStoredState,
    HBoxWallet,
    SimpleBoxTransactionMemPool,
    GeneratorInfo] = {
    val f: CurrentView[HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionMemPool] => GeneratorInfo = {
      view: CurrentView[HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionMemPool] =>
        GeneratorInfo(generate(view.vault))
    }
    GetDataFromCurrentView[HybridHistory,
      HBoxStoredState,
      HBoxWallet,
      SimpleBoxTransactionMemPool,
      GeneratorInfo](f)
  }


  override def receive: Receive = {
    case StartGeneration(duration) =>
      context.system.scheduler.schedule(duration, duration, viewHolderRef, getRequiredData)

    //    case CurrentView(_, _, wallet: HWallet, _) =>
    case gi: GeneratorInfo =>
      gi.tx match {
        case Success(tx) =>
          log.info(s"Local tx with with ${tx.from.size} inputs, ${tx.to.size} outputs. Valid: ${tx.semanticValidity}")
          viewHolderRef ! LocallyGeneratedTransaction[SimpleBoxTransaction](tx)
        case Failure(e) =>
          e.printStackTrace()
      }
  }

  private val ex: ArrayBuffer[Array[Byte]] = ArrayBuffer()

  def generate(wallet: HBoxWallet): Try[SimpleBoxTransaction] = {
    if (Random.nextInt(100) == 1) ex.clear()

    val pubkeys = wallet.publicKeys.toSeq
    if (pubkeys.size < 10) wallet.generateNewSecret()
    val recipients = scala.util.Random.shuffle(pubkeys).take(Random.nextInt(pubkeys.size))
      .map(r => (r, Value @@ Random.nextInt(100).toLong))
    val tx = SimpleBoxTransaction.create(wallet, recipients, Random.nextInt(100), ex)
    tx.map(t => t.boxIdsToOpen.foreach(id => ex += id))
    tx
  }
}

object SimpleBoxTransactionGenerator {

  object ReceivableMessages {

    case class StartGeneration(delay: FiniteDuration)

    case class GeneratorInfo(tx: Try[SimpleBoxTransaction])

  }

}

object SimpleBoxTransactionGeneratorRef {
  def props(viewHolderRef: ActorRef)(implicit ec: ExecutionContext): Props =
    Props(new SimpleBoxTransactionGenerator(viewHolderRef))

  def apply(viewHolderRef: ActorRef)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = system.actorOf(props(viewHolderRef))

  def apply(name: String, viewHolderRef: ActorRef)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = system.actorOf(props(viewHolderRef), name)
}
