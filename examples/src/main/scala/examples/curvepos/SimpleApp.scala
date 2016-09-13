package examples.curvepos

import scorex.core.NodeViewHolder
import scorex.core.api.http.ApiRoute
import scorex.core.app.{Application, ApplicationVersion}
import scorex.core.network.message.MessageSpec
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.wallet.Wallet

/**
  * Curve25519 accounts + ??? Consensus
  */
class SimpleApp extends Application {
  override val applicationName: String = "SimpleApp"

  override lazy val appVersion: ApplicationVersion = ApplicationVersion(0, 1, 0)

  override type P = PublicKey25519Proposition
  override type TX = Nothing
  override type PMOD = Nothing

  override protected val additionalMessageSpecs: Seq[MessageSpec[_]] = null
  override val apiTypes = null
  override val wallet: Wallet[P, TX, _] = null
  override val apiRoutes: Seq[ApiRoute] = null
  override val nodeViewHolder: NodeViewHolder[P, TX, PMOD] = null
  override implicit val settings: Settings = null
}
