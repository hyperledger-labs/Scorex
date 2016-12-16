package scorex.core.app

import akka.actor.{ActorRef, ActorSystem, Props}
import scorex.core.{NodeViewHolder, PersistentNodeViewModifier}
import scorex.core.network._
import scorex.core.network.message._
import scorex.core.network.peer.PeerManager
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.Transaction
import scorex.core.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global

trait Application extends ScorexLogging {

  type P <: Proposition
  type TX <: Transaction[P]
  type PMOD <: PersistentNodeViewModifier[P, TX]
  type NVHT <: NodeViewHolder[P, TX, PMOD]

  val ApplicationNameLimit = 50

  //settings
  implicit val settings: Settings

  protected implicit lazy val actorSystem = ActorSystem(settings.agentName)

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

  val nodeViewHolderRef: ActorRef
  val nodeViewSynchronizer: ActorRef
  val localInterface: ActorRef


  val peerManagerRef = actorSystem.actorOf(Props(classOf[PeerManager], settings))

  val nProps = Props(classOf[NetworkController], settings, messagesHandler, upnp, peerManagerRef)
  val networkController = actorSystem.actorOf(nProps, "networkController")

  def run(): Unit = {
    require(settings.agentName.length <= ApplicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")

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
