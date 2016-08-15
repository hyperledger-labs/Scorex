package scorex.app

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import scorex.NodeStateHolder
import scorex.api.http.{ApiRoute, CompositeHttpService}
import scorex.block._
import scorex.consensus.ConsensusModule
import scorex.network._
import scorex.network.message._
import scorex.serialization.BytesParseable
import scorex.settings.Settings
import scorex.transaction.box.proposition.Proposition
import scorex.transaction.{Transaction, TransactionalModule}
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe.Type

trait Application extends ScorexLogging {
  val ApplicationNameLimit = 50

  val applicationName: String

  //redefine it as lazy val
  def appVersion: ApplicationVersion

  type P <: Proposition
  type TX <: Transaction[P, TX]
  type CD <: ConsensusData
  type TD <: TransactionalData[TX]

  //settings
  implicit val settings: Settings

  //modules
  implicit val consensusModule: ConsensusModule[P, CD]
  implicit val transactionalModule: TransactionalModule[P, TX, TD]

  val consensusParser: BytesParseable[CD]
  val transactionalParser: BytesParseable[TD]

  val blockValidator: BlockValidator[P, TX, TD, CD]

  val stateHolder: NodeStateHolder[P, TX, TD, CD]

  val rewardCalculator: StateChangesCalculator[P, TX, TD, CD]

  lazy val wallet = transactionalModule.wallet

  type BType = Block[P, TD, CD]

  //api
  val apiRoutes: Seq[ApiRoute]
  val apiTypes: Seq[Type]

  protected implicit lazy val actorSystem = ActorSystem("lagonaki")

  protected val additionalMessageSpecs: Seq[MessageSpec[_]]

  //p2p
  lazy val upnp = new UPnP(settings)

  val blockMessageSpec = new BlockMessageSpec[P, TX, TD, CD](consensusParser, transactionalParser)

  private lazy val basicSpecs =
    Seq(
      GetPeersSpec,
      PeersSpec,
      GetSignaturesSpec,
      SignaturesSpec,
      GetBlockSpec,
      blockMessageSpec,
      ScoreMessageSpec
    )

  lazy val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs)


  lazy val networkController = actorSystem.actorOf(Props(classOf[NetworkController], settings, messagesHandler, upnp,
    applicationName, appVersion), "networkController")

  lazy val historySynchronizer = actorSystem.actorOf(Props(classOf[HistorySynchronizer[P, TX, TD, CD]], settings,
    stateHolder, networkController, blockMessageSpec, blockValidator, rewardCalculator, consensusModule,
    transactionalModule), "HistorySynchronizer")

  implicit val materializer = ActorMaterializer()
  lazy val combinedRoute = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings).compositeRoute


  def run() {
    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")

    // todo: fix
    // checkGenesis()

    Http().bindAndHandle(combinedRoute, "0.0.0.0", settings.rpcPort)

    historySynchronizer ! Unit

    //on unexpected shutdown
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        log.error("Unexpected shutdown")
        stopAll()
      }
    })
  }

  def stopAll(): Unit = synchronized {
    log.info("Stopping network services")
    if (settings.upnpEnabled) upnp.deletePort(settings.port)
    networkController ! NetworkController.ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")
    actorSystem.terminate().onComplete { _ =>
      log.info("Closing wallet")
      transactionalModule.stop()

      log.info("Exiting from the app...")
      System.exit(0)
    }
  }
}
