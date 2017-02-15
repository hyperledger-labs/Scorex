package examples.hybrid

import akka.actor.{ActorRef, Props}
import examples.hybrid.api.http.{DebugApiRoute, WalletApiRoute}
import examples.hybrid.blocks.HybridBlock
import examples.hybrid.history.{HybridSyncInfo, HybridSyncInfoMessageSpec}
import examples.hybrid.mining.{MiningSettings, PosForger, PowMiner}
import examples.hybrid.state.SimpleBoxTransaction
import examples.hybrid.wallet.SimpleBoxTransactionGenerator
import examples.hybrid.wallet.SimpleBoxTransactionGenerator.StartGeneration
import io.circe
import scorex.core.api.http.{ApiRoute, NodeViewApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageSpec
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.concurrent.duration._
import scala.reflect.runtime.universe._

class HybridApp(val settingsFilename: String) extends Application {

  override type P = PublicKey25519Proposition
  override type TX = SimpleBoxTransaction
  override type PMOD = HybridBlock
  override type NVHT = HybridNodeViewHolder

  implicit lazy val settings = new MiningSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }
  log.debug(s"Starting application with settings \n$settings")

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(HybridSyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(classOf[HybridNodeViewHolder], settings))

  override val apiRoutes: Seq[ApiRoute] = Seq(
    DebugApiRoute(settings, nodeViewHolderRef),
    WalletApiRoute(settings, nodeViewHolderRef),
    UtilsApiRoute(settings),
    NodeViewApiRoute[P, TX](settings, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkController, settings)
  )

  override val apiTypes: Seq[Type] = Seq(typeOf[UtilsApiRoute], typeOf[DebugApiRoute], typeOf[WalletApiRoute],
    typeOf[NodeViewApiRoute[P, TX]], typeOf[PeersApiRoute])

  val miner = actorSystem.actorOf(Props(classOf[PowMiner], nodeViewHolderRef, settings))
  val forger = actorSystem.actorOf(Props(classOf[PosForger], settings, nodeViewHolderRef))

  override val localInterface: ActorRef = actorSystem.actorOf(Props(classOf[HLocalInterface], nodeViewHolderRef, miner, forger, settings))

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(Props(classOf[NodeViewSynchronizer[P, TX, HybridSyncInfo, HybridSyncInfoMessageSpec.type]],
      networkController, nodeViewHolderRef, localInterface, HybridSyncInfoMessageSpec))

  //touching lazy vals
  miner
  localInterface
  nodeViewSynchronizer

  if (settings.nodeName == "node1") {
    log.info("Starting transactions generation")
    val generator: ActorRef = actorSystem.actorOf(Props(classOf[SimpleBoxTransactionGenerator], nodeViewHolderRef))
    generator ! StartGeneration(FiniteDuration(10, SECONDS))
  }
}

object HybridApp extends App {
  val settingsFilename = args.headOption.getOrElse("settings.json")
  new HybridApp(settingsFilename).run()
}