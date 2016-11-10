package scorex.core.app

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import scorex.core.api.http.{ApiRoute, CompositeHttpService}
import scorex.core.network._
import scorex.core.network.message._
import scorex.core.serialization.ScorexKryoPool
import scorex.core.settings.Settings
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging
import scorex.core.{NodeViewHolder, PersistentNodeViewModifier}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe.Type

trait Application extends ScorexLogging {
  val ApplicationNameLimit = 50

  val applicationName: String

  //redefine it as lazy val
  def appVersion: ApplicationVersion

  type P <: Proposition
  type TX <: Transaction[P]
  type PMOD <: PersistentNodeViewModifier[P, TX]
  type NVHT <: NodeViewHolder[P, TX, PMOD]

  //settings
  implicit val settings: Settings

  //api
  val apiRoutes: Seq[ApiRoute]
  val apiTypes: Seq[Type]

  val serializer: ScorexKryoPool
  protected implicit lazy val actorSystem = ActorSystem(applicationName)

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

  lazy val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs, serializer)

  val nodeViewHolderRef: ActorRef

  lazy val networkController = actorSystem.actorOf(Props(classOf[NetworkController], settings, messagesHandler, upnp,
    applicationName, appVersion, serializer), "networkController")

  val nodeViewSynchronizer: ActorRef

  val localInterface: ActorRef

  implicit val materializer = ActorMaterializer()
  lazy val combinedRoute = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings).compositeRoute


  def run() {
    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at 0.0.0.0:${settings.rpcPort}")

    Http().bindAndHandle(combinedRoute, "0.0.0.0", settings.rpcPort)

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
