package scorex.core.app

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import scorex.core.NodeViewHolder
import scorex.core.api.http.{ApiRoute, CompositeHttpService}
import scorex.core.network._
import scorex.core.network.message._
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.wallet.Wallet
import scorex.core.transaction.{PersistentNodeViewModifier, Transaction}
import scorex.core.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe.Type

trait Application extends ScorexLogging {
  val ApplicationNameLimit = 50

  val applicationName: String

  //redefine it as lazy val
  def appVersion: ApplicationVersion

  type P <: Proposition
  type TX <: Transaction[P, TX]
  type PMOD <: PersistentNodeViewModifier[P, TX]

  //settings
  implicit val settings: Settings

  val nodeViewHolder: NodeViewHolder[P, TX, PMOD]

  val wallet: Wallet[P, TX, _]

  //api
  val apiRoutes: Seq[ApiRoute]
  val apiTypes: Seq[Type]

  protected implicit lazy val actorSystem = ActorSystem("lagonaki")

  protected val additionalMessageSpecs: Seq[MessageSpec[_]]

  //p2p
  lazy val upnp = new UPnP(settings)

  private lazy val basicSpecs =
    Seq(
      GetPeersSpec,
      PeersSpec,
      InvSpec,
      RequestModifierSpec,
      ModifiersSpec
    )

  lazy val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs)


  lazy val networkController = actorSystem.actorOf(Props(classOf[NetworkController], settings, messagesHandler, upnp,
    applicationName, appVersion), "networkController")

  //lazy val historySynchronizer = actorSystem.actorOf(Props(classOf[HistorySynchronizer[P, TX, B]], settings,
//    stateHolder, networkController, wallet), "HistorySynchronizer")

  implicit val materializer = ActorMaterializer()
  lazy val combinedRoute = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings).compositeRoute


  def run() {
    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")

    Http().bindAndHandle(combinedRoute, "0.0.0.0", settings.rpcPort)

    //historySynchronizer ! scorex.block.Block.genesis[P, TX, CD](settings.genesisTimestamp)

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

      log.info("Exiting from the app...")
      System.exit(0)
    }
  }
}
