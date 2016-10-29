package examples.hybrid

import akka.actor.ActorRef
import examples.curvepos.forging.ForgerSettings
import examples.hybrid.blocks.HybridPersistentNodeViewModifier
import examples.hybrid.state.SimpleBoxTransaction
import io.circe
import scorex.core.api.http.ApiRoute
import scorex.core.app.{Application, ApplicationVersion}
import scorex.core.network.message.MessageSpec
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.reflect.runtime.universe._

class HybridApp(val settingsFilename: String) extends Application {
  implicit lazy val settings = new Settings with ForgerSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }

  override val applicationName: String = "2-Hop"

  //redefine it as lazy val
  override def appVersion: ApplicationVersion = ApplicationVersion(0, 1, 1)

  override type P = PublicKey25519Proposition
  override type TX = SimpleBoxTransaction
  override type PMOD = HybridPersistentNodeViewModifier
  override type NVHT = HybridNodeViewHolder

  override val apiRoutes: Seq[ApiRoute] = ???
  override val apiTypes: Seq[Type] = ???

  override protected val additionalMessageSpecs: Seq[MessageSpec[_]] = ???

  override val nodeViewHolderRef: ActorRef = ???

  override val nodeViewSynchronizer: ActorRef = ???
  override val localInterface: ActorRef = ???
}


object HybridApp extends App {
  val settingsFilename = args.headOption.getOrElse("settings.json")
  new HybridApp(settingsFilename).run()
}