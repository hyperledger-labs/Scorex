package scorex.app

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import scorex.api.http.{ApiRoute, CompositeHttpService}
import scorex.block.{Block, ConsensusData, TransactionalData}
import scorex.consensus.ConsensusModule
import scorex.network._
import scorex.network.message.{BasicMessagesRepo, MessageHandler, MessageSpec}
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
  type CData <: ConsensusData
  type TData <: TransactionalData[TX]

  //settings
  implicit val settings: Settings

  //modules
  implicit val consensusModule: ConsensusModule[P, TX, TData, CData]
  implicit val transactionModule: TransactionalModule[P, TX, TData]

  lazy val wallet = transactionModule.wallet

  type BType = Block[P, TData, CData]

  //api
  val apiRoutes: Seq[ApiRoute]
  val apiTypes: Seq[Type]

  protected implicit lazy val actorSystem = ActorSystem("lagonaki")

  protected val additionalMessageSpecs: Seq[MessageSpec[_]]

  lazy val basicMessagesSpecsRepo = new BasicMessagesRepo[P, TX, TData, CData]()

  //p2p
  lazy val upnp = new UPnP(settings)

  lazy val messagesHandler: MessageHandler = MessageHandler(basicMessagesSpecsRepo.specs ++ additionalMessageSpecs)


  lazy val networkController = actorSystem.actorOf(Props(classOf[NetworkController], settings, messagesHandler, upnp,
    applicationName, appVersion), "networkController")

  lazy val historySynchronizer = actorSystem.actorOf(Props(classOf[HistorySynchronizer[P, TX, TData, CData]], settings,
    consensusModule, networkController, basicMessagesSpecsRepo), "HistorySynchronizer")

  lazy val historyReplier = actorSystem.actorOf(Props(classOf[HistoryReplier[P, TX, TData, CData]], settings,
    basicMessagesSpecsRepo, networkController, consensusModule), "HistoryReplier")


  implicit val materializer = ActorMaterializer()
  lazy val combinedRoute = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings).compositeRoute


  def run() {
    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")

    checkGenesis()

    Http().bindAndHandle(combinedRoute, "0.0.0.0", settings.rpcPort)

    historySynchronizer ! Unit
    historyReplier ! Unit

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
      transactionModule.stop()

      log.info("Exiting from the app...")
      System.exit(0)
    }
  }

  def checkGenesis(): Unit = {
    if (consensusModule.isEmpty) {
      val genesisBlock: BType = Block.genesis[P, TX, TData, CData](settings.genesisTimestamp)
      consensusModule.appendBlock(genesisBlock)
      log.info("Genesis block has been added to the state")
    }
  }.ensuring(consensusModule.height() >= 1)
}
