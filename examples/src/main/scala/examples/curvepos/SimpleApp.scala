package examples.curvepos

import examples.curvepos.transaction.{SimpleBlock, SimpleWallet, SimplestTransaction}
import io.circe.Json
import scorex.core.NodeViewHolder
import scorex.core.api.http.ApiRoute
import scorex.core.app.{Application, ApplicationVersion}
import scorex.core.network.message.MessageSpec
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.wallet.Wallet
import scorex.utils.Random

/**
  * Curve25519 accounts + ??? Consensus
  */
class SimpleApp extends Application {
  override implicit val settings: Settings = new Settings {
    override def settingsJSON: Map[String, Json] = Map()
  }

  override val applicationName: String = "SimpleApp"

  override lazy val appVersion: ApplicationVersion = ApplicationVersion(0, 1, 0)

  override type P = PublicKey25519Proposition
  override type TX = SimplestTransaction
  override type PMOD = SimpleBlock

  override protected val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq()
  override val apiTypes = Seq()
  override val apiRoutes: Seq[ApiRoute] = Seq()
  override val wallet: Wallet[P, TX, _] = new SimpleWallet(Random.randomBytes(32))
  override val nodeViewHolder: NodeViewHolder[P, TX, PMOD] = new SimpleNodeViewHolder
}

object SimpleApp extends App {
  val app = new SimpleApp
  app.run()
}