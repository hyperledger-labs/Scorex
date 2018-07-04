package scorex.core.app

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.ExceptionHandler
import akka.stream.ActorMaterializer
import scorex.core.api.http.{ApiErrorHandler, ApiRoute, CompositeHttpService}
import scorex.core.network._
import scorex.core.network.message._
import scorex.core.network.peer.PeerManagerRef
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.core.{NodeViewHolder, PersistentNodeViewModifier}

import scala.concurrent.ExecutionContext

trait Application extends ScorexLogging {

  import scorex.core.network.NetworkController.ReceivableMessages.ShutdownNetwork

  type P <: Proposition
  type TX <: Transaction
  type PMOD <: PersistentNodeViewModifier
  type NVHT <: NodeViewHolder[TX, PMOD]

  val ApplicationNameLimit = 50

  //settings
  implicit val settings: ScorexSettings

  //api
  val apiRoutes: Seq[ApiRoute]

  implicit def exceptionHandler: ExceptionHandler = ApiErrorHandler.exceptionHandler

  protected implicit lazy val actorSystem = ActorSystem(settings.network.agentName)
  implicit val executionContext: ExecutionContext = actorSystem.dispatchers.lookup("scorex.executionContext")

  protected val features: Seq[PeerFeature]
  protected val additionalMessageSpecs: Seq[MessageSpec[_]]

  //p2p
  lazy val upnp = new UPnP(settings.network)

  private lazy val basicSpecs = {
    val invSpec = new InvSpec(settings.network.maxInvObjects)
    val requestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
    Seq(
      GetPeersSpec,
      PeersSpec,
      invSpec,
      requestModifierSpec,
      ModifiersSpec
    )
  }

  lazy val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs)

  val nodeViewHolderRef: ActorRef
  val nodeViewSynchronizer: ActorRef
  /**
    * API description in openapi format in YAML or JSON
    */
  val swaggerConfig: String

  val timeProvider = new NetworkTimeProvider(settings.ntp)


  val peerManagerRef = PeerManagerRef(settings, timeProvider)

  val networkControllerRef: ActorRef = NetworkControllerRef("networkController", settings.network,
                                                            messagesHandler, features, upnp,
                                                            peerManagerRef, timeProvider)

  lazy val combinedRoute = CompositeHttpService(actorSystem, apiRoutes, settings.restApi, swaggerConfig).compositeRoute

  def run(): Unit = {
    require(settings.network.agentName.length <= ApplicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at ${settings.restApi.bindAddress.toString}")

    implicit val materializer = ActorMaterializer()
    val bindAddress = settings.restApi.bindAddress

    Http().bindAndHandle(combinedRoute, bindAddress.getAddress.getHostAddress, bindAddress.getPort)

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
    if (settings.network.upnpEnabled) upnp.deletePort(settings.network.bindAddress.getPort)
    networkControllerRef ! ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")
    actorSystem.terminate().onComplete { _ =>

      log.info("Exiting from the app...")
      System.exit(0)
    }
  }
}
