package examples.hybrid

import akka.actor.{ActorRef, Props}
import examples.hybrid.blocks.HybridPersistentNodeViewModifier
import examples.hybrid.history.{HybridSyncInfo, HybridSyncInfoSpec}
import examples.hybrid.mining.{MiningSettings, PosForger, PowMiner}
import examples.hybrid.state.SimpleBoxTransaction
import examples.hybrid.wallet.SimpleBoxTransactionGenerator
import examples.hybrid.wallet.SimpleBoxTransactionGenerator.StartGeneration
import io.circe
import scorex.core.api.http.{ApiRoute, NodeViewApiRoute, UtilsApiRoute}
import scorex.core.app.{Application, ApplicationVersion}
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageSpec
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.concurrent.duration._
import scala.reflect.runtime.universe._

class HybridApp(val settingsFilename: String) extends Application {
  implicit lazy val settings = new Settings with MiningSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }

  override lazy val applicationName: String = "2-Hop"

  //redefine it as lazy val
  override def appVersion: ApplicationVersion = ApplicationVersion(0, 1, 1)

  override type P = PublicKey25519Proposition
  override type TX = SimpleBoxTransaction
  override type PMOD = HybridPersistentNodeViewModifier
  override type NVHT = HybridNodeViewHolder

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(HybridSyncInfoSpec)

  override val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(classOf[HybridNodeViewHolder], settings))

  override val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings),
    NodeViewApiRoute[P, TX](settings, nodeViewHolderRef))

  override val apiTypes: Seq[Type] = Seq(typeOf[UtilsApiRoute], typeOf[NodeViewApiRoute[P, TX]])

  val miner = actorSystem.actorOf(Props(classOf[PowMiner], nodeViewHolderRef, settings))
  val forger = actorSystem.actorOf(Props(classOf[PosForger], settings, nodeViewHolderRef))

  override val localInterface: ActorRef = actorSystem.actorOf(Props(classOf[HLocalInterface], nodeViewHolderRef, miner, forger))

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(Props(classOf[NodeViewSynchronizer[P, TX, HybridSyncInfo, HybridSyncInfoSpec.type]],
      networkController, nodeViewHolderRef, localInterface, HybridSyncInfoSpec))

  //touching lazy vals
  miner
  localInterface
  nodeViewSynchronizer

  if(settings.nodeName == "node1") {
    log.info("Starting transactions generation")
    val generator: ActorRef = actorSystem.actorOf(Props(classOf[SimpleBoxTransactionGenerator], nodeViewHolderRef))
    generator ! StartGeneration(FiniteDuration(10, SECONDS))
  }
}

object HybridApp extends App {
  val settingsFilename = args.headOption.getOrElse("settings.json")
  new HybridApp(settingsFilename).run()
}