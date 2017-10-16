package examples.hybrid

import akka.actor.{ActorRef, Props}
import examples.commons.SimpleBoxTransaction
import examples.hybrid.api.http.{DebugApiRoute, StatsApiRoute, WalletApiRoute}
import examples.hybrid.blocks.HybridBlock
import examples.hybrid.history.{HybridSyncInfo, HybridSyncInfoMessageSpec}
import examples.hybrid.mining.{HybridSettings, PosForger, PowMiner}
import examples.hybrid.wallet.SimpleBoxTransactionGenerator
import examples.hybrid.wallet.SimpleBoxTransactionGenerator.StartGeneration
import scorex.core.api.http.{ApiRoute, NodeViewApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageSpec
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.concurrent.duration._

class HybridApp(val settingsFilename: String) extends Application {

  override type P = PublicKey25519Proposition
  override type TX = SimpleBoxTransaction
  override type PMOD = HybridBlock
  override type NVHT = HybridNodeViewHolder

  private val hybridSettings = HybridSettings.read(Some(settingsFilename))
  implicit lazy val settings = hybridSettings.scorexSettings

  log.debug(s"Starting application with settings \n$settings")

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(HybridSyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new HybridNodeViewHolder(settings, hybridSettings.mining)))

  override val apiRoutes: Seq[ApiRoute] = Seq(
    DebugApiRoute(settings.restApi, nodeViewHolderRef),
    WalletApiRoute(settings.restApi, nodeViewHolderRef),
    StatsApiRoute(settings.restApi, nodeViewHolderRef),
    UtilsApiRoute(settings.restApi),
    NodeViewApiRoute[P, TX](settings.restApi, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkController, settings.restApi)
  )

  override val apiTypes: Set[Class[_]] = Set(classOf[UtilsApiRoute], classOf[DebugApiRoute], classOf[WalletApiRoute],
    classOf[NodeViewApiRoute[P, TX]], classOf[PeersApiRoute], classOf[StatsApiRoute])

  val miner = actorSystem.actorOf(Props(new PowMiner(nodeViewHolderRef, hybridSettings.mining)))
  val forger = actorSystem.actorOf(Props(new PosForger(hybridSettings, nodeViewHolderRef)))

  override val localInterface: ActorRef = actorSystem.actorOf(Props(new HLocalInterface(nodeViewHolderRef, miner, forger, hybridSettings.mining)))

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(Props(new NodeViewSynchronizer[P, TX, HybridSyncInfo, HybridSyncInfoMessageSpec.type]
    (networkController, nodeViewHolderRef, localInterface, HybridSyncInfoMessageSpec, settings.network)))

  //touching lazy vals
  miner
  localInterface
  nodeViewSynchronizer

  if (settings.network.nodeName.startsWith("generatorNode")) {
    log.info("Starting transactions generation")
    val generator: ActorRef = actorSystem.actorOf(Props(classOf[SimpleBoxTransactionGenerator], nodeViewHolderRef))
    generator ! StartGeneration(FiniteDuration(10, SECONDS))
  }
}

object HybridApp extends App {
  val settingsFilename = args.headOption.getOrElse("settings.conf")
  new HybridApp(settingsFilename).run()
}
