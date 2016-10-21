package io.scalac.elm

import akka.actor.ActorRef
import io.scalac.elm.config.{AppConfig, AppInfo}
import io.scalac.elm.core.ElmNodeViewHolder
import io.scalac.elm.transaction.{ElmBlock, ElmTransaction}
import scorex.core.api.http._
import scorex.core.app.Application
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.wallet.Wallet
import scorex.core.utils.ScorexLogging

import scala.reflect.runtime.universe.typeOf

class ElmApp(appConfig: AppConfig) extends Application {
  override val applicationName = AppInfo.name
  override val appVersion = AppInfo.appVersion

  override val settings = appConfig.settings

  override type P = PublicKey25519Proposition
  override type TX = ElmTransaction
  override type PMOD = ElmBlock
  override type NVHT = ElmNodeViewHolder

  override val apiRoutes = Seq(
    UtilsApiRoute(settings),
    PeersApiRoute(peerManager, networkController, settings)
  )

  override val apiTypes = Seq(
    typeOf[UtilsApiRoute],
    typeOf[PeersApiRoute]
  )

  override protected val additionalMessageSpecs = Nil

  override val wallet: Wallet[P, TX, _] = null
  override val nodeViewHolderRef: ActorRef = null
  override val nodeViewSynchronizer: ActorRef = null
  override val localInterface: ActorRef = null
}

object ElmApp extends App with ScorexLogging {
  val elmApp = new ElmApp(AppConfig.load())
  elmApp.run()
}
