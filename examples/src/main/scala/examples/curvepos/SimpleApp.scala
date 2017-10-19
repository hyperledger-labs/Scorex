package examples.curvepos

import akka.actor.{ActorRef, Props}
import examples.curvepos.forging.Forger
import examples.curvepos.transaction.{SimpleBlock, SimpleTransaction}
import io.circe
import scorex.core.api.http.{ApiRoute, NodeViewApiRoute, UtilsApiRoute}
import scorex.core.app.{Application, Version}
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.reflect.runtime.universe._

/**
  * Simple application implementing simple transactions
  * (just transfers from one pubkey to another)
  * and Nxt-like(simplified) Consensus
  */
class SimpleApp(val settingsFilename: String) extends Application {
  implicit lazy val settings = ScorexSettings.read(Some(settingsFilename))

  override type P = PublicKey25519Proposition
  override type TX = SimpleTransaction
  override type PMOD = SimpleBlock

  override type NVHT = SimpleNodeViewHolder

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(SimpleSyncInfoMessageSpec)

  override lazy val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(classOf[SimpleNodeViewHolder], settings))

  val forger = actorSystem.actorOf(Props(classOf[Forger], nodeViewHolderRef, settings))

  override val localInterface: ActorRef = actorSystem.actorOf(Props(classOf[SimpleLocalInterface], nodeViewHolderRef,
    forger))

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(Props(classOf[NodeViewSynchronizer[P, TX, SimpleSyncInfo, SimpleSyncInfoMessageSpec.type]],
      networkController, nodeViewHolderRef, localInterface, SimpleSyncInfoMessageSpec))

  override val apiTypes: Set[Class[_]] = Set(classOf[UtilsApiRoute], classOf[NodeViewApiRoute[P, TX]])
  override val apiRoutes: Seq[ApiRoute] = Seq(UtilsApiRoute(settings.restApi),
    NodeViewApiRoute[P, TX](settings.restApi, nodeViewHolderRef))
}

object SimpleApp extends App {
  val settingsFilename = args.headOption.getOrElse("settings.conf")
  new SimpleApp(settingsFilename).run()
}
