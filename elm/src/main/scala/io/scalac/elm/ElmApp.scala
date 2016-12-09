package io.scalac.elm

import akka.actor.Props
import io.scalac.elm.api.{BlockchainApiRoute, WalletApiRoute}
import io.scalac.elm.config.{AppConfig, AppInfo}
import io.scalac.elm.consensus.{ElmSyncInfo, ElmSyncInfoSpec}
import io.scalac.elm.core.{ElmLocalInterface, ElmNodeViewHolder}
import io.scalac.elm.state.ElmWallet
import io.scalac.elm.transaction.{ElmBlock, ElmTransaction}
import scorex.core.api.http._
import scorex.core.app.Application
import scorex.core.network.NodeViewSynchronizer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging

import scala.reflect.runtime.universe.typeOf

class ElmApp(appConfig: AppConfig) extends {

  override val applicationName = AppInfo.name
  override val appVersion = AppInfo.appVersion
  override val settings = appConfig.settings
  override protected val additionalMessageSpecs = Nil

} with Application {

  override type P = PublicKey25519Proposition
  override type TX = ElmTransaction
  override type PMOD = ElmBlock
  override type NVHT = ElmNodeViewHolder

  override val wallet = ElmWallet() // ???
  override val nodeViewHolderRef = actorSystem.actorOf(Props(classOf[ElmNodeViewHolder], appConfig))
  override val nodeViewSynchronizer = actorSystem.actorOf(
    Props(classOf[NodeViewSynchronizer[P, TX, ElmSyncInfo, ElmSyncInfoSpec.type]],
      networkController, nodeViewHolderRef, ElmSyncInfoSpec))
  override val localInterface = actorSystem.actorOf(Props(classOf[ElmLocalInterface], nodeViewHolderRef))

  override val apiRoutes = Seq(
    UtilsApiRoute(settings),
    PeersApiRoute(peerManager, networkController, settings),
    new WalletApiRoute(settings, nodeViewHolderRef),
    new BlockchainApiRoute(settings, nodeViewHolderRef)
  )

  override val apiTypes = Seq(
    typeOf[UtilsApiRoute],
    typeOf[PeersApiRoute],
    typeOf[WalletApiRoute],
    typeOf[BlockchainApiRoute]
  )
}

object ElmApp extends App with ScorexLogging {
  val elmApp = new ElmApp(AppConfig.load())
  elmApp.run()
}
