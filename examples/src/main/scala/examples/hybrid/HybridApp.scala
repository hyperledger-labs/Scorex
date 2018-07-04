package examples.hybrid

import akka.actor.ActorRef
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.hybrid.api.http.{DebugApiRoute, StatsApiRoute, WalletApiRoute}
import examples.hybrid.blocks.HybridBlock
import examples.hybrid.history.{HybridHistory, HybridSyncInfo, HybridSyncInfoMessageSpec}
import examples.hybrid.mining._
import examples.hybrid.wallet.SimpleBoxTransactionGeneratorRef
import scorex.core.api.http.{ApiRoute, NodeViewApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.{PeerFeature, NodeViewSynchronizerRef}
import scorex.core.network.message.MessageSpec
import scorex.core.serialization.SerializerRegistry
import scorex.core.serialization.SerializerRegistry.SerializerRecord
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

class HybridApp(val settingsFilename: String) extends Application {

  import examples.hybrid.wallet.SimpleBoxTransactionGenerator.ReceivableMessages.StartGeneration

  override type P = PublicKey25519Proposition
  override type TX = SimpleBoxTransaction
  override type PMOD = HybridBlock
  override type NVHT = HybridNodeViewHolder

  private val hybridSettings = HybridSettings.read(Some(settingsFilename))
  implicit override lazy val settings: ScorexSettings = HybridSettings.read(Some(settingsFilename)).scorexSettings

  log.debug(s"Starting application with settings \n$settings")

  implicit val serializerReg: SerializerRegistry = SerializerRegistry(Seq(SerializerRecord(SimpleBoxTransaction.simpleBoxEncoder)))

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(HybridSyncInfoMessageSpec)

  override protected lazy val features: Seq[PeerFeature] = Seq()

  override val nodeViewHolderRef: ActorRef = HybridNodeViewHolderRef(hybridSettings, timeProvider)

  override val apiRoutes: Seq[ApiRoute] = Seq[ApiRoute](
    DebugApiRoute(settings.restApi, nodeViewHolderRef),
    WalletApiRoute(settings.restApi, nodeViewHolderRef),
    StatsApiRoute(settings.restApi, nodeViewHolderRef),
    UtilsApiRoute(settings.restApi),
    NodeViewApiRoute[SimpleBoxTransaction](settings.restApi, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkControllerRef, settings.restApi)
  )

  override val swaggerConfig: String = Source.fromResource("api/testApi.yaml").getLines.mkString("\n")

  val miner: ActorRef = PowMinerRef(nodeViewHolderRef, hybridSettings.mining)
  val forger: ActorRef = PosForgerRef(hybridSettings, nodeViewHolderRef)

  val localInterface: ActorRef = HLocalInterfaceRef(nodeViewHolderRef, miner, forger, hybridSettings.mining)

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(NodeViewSynchronizerRef.props[SimpleBoxTransaction, HybridSyncInfo, HybridSyncInfoMessageSpec.type,
      HybridBlock, HybridHistory, SimpleBoxTransactionMemPool]
      (networkControllerRef, nodeViewHolderRef,
        HybridSyncInfoMessageSpec, settings.network, timeProvider))

  if (settings.network.nodeName.startsWith("generatorNode")) {
    log.info("Starting transactions generation")
    val generator: ActorRef = SimpleBoxTransactionGeneratorRef(nodeViewHolderRef)
    generator ! StartGeneration(10 seconds)
  }
}

object HybridApp extends App {
  private val settingsFilename = args.headOption.getOrElse("settings.conf")
  new HybridApp(settingsFilename).run()
}
