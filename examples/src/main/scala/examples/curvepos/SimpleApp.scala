package examples.curvepos

import akka.actor.{ActorRef, Props}
import examples.curvepos.transaction.{SimpleBlock, SimpleTransaction, SimpleWallet}
import io.circe
import scorex.core.api.http.{ApiRoute, UtilsApiRoute}
import scorex.core.app.{Application, ApplicationVersion}
import scorex.core.network.message.MessageSpec
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.wallet.Wallet
import scala.reflect.runtime.universe._

/**
  * Simple application implementing simple transactions
  * (just transfers from one pubkey to another)
  * and Nxt-like(simplified) Consensus
  */
class SimpleApp extends Application {
  val settingsFilename = "settings.json"
  implicit val settings = new Settings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }

  override lazy val applicationName: String = "SimpleApp"

  override lazy val appVersion: ApplicationVersion = ApplicationVersion(0, 1, 0)

  override type P = PublicKey25519Proposition
  override type TX = SimpleTransaction
  override type PMOD = SimpleBlock

  override type NVHT = SimpleNodeViewHolder

  override protected val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq()
  override val apiTypes: Seq[Type] = Seq(typeOf[UtilsApiRoute])
  override val apiRoutes: Seq[ApiRoute] = Seq(UtilsApiRoute(settings))
  override val wallet: Wallet[P, TX, _] = SimpleWallet()

  override lazy val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(classOf[SimpleNodeViewHolder]))
}

object SimpleApp extends App {
  new SimpleApp().run()
}