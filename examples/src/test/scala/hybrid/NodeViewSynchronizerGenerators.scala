package hybrid

import akka.actor.{ActorRef, ActorSystem, Props}
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.hybrid.HybridNodeViewHolder
import examples.hybrid.blocks._
import examples.hybrid.history.HybridHistory
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.VersionTag
import scorex.core.consensus.SyncInfo
import scorex.core.network.ConnectedPeer
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ByteStr

trait NodeViewSynchronizerGenerators { this: ModifierGenerators with StateGenerators with HistoryGenerators =>

  //type P = PublicKey25519Proposition
  //type TX = SimpleBoxTransaction
  type PM = HybridBlock
  type SI = SyncInfo
  type SIS = SyncInfoMessageSpec[SI]

  //type NODE = HybridNodeViewHolder
  //type ST = HBoxStoredState
  //type HT = HybridHistory


  object NodeViewSynchronizerForTests {
    // todo: there is nothing here that is special to tests. Should this `props` method be moved to NodeViewSynchronizer's companion object?
    def props(networkControllerRef: ActorRef,
              viewHolderRef: ActorRef,
              localInterfaceRef: ActorRef,
              syncInfoSpec: SIS,
              networkSettings: NetworkSettings): Props =
      Props(new NodeViewSynchronizer(networkControllerRef,
        viewHolderRef,
        localInterfaceRef,
        syncInfoSpec,
        networkSettings))
  }

  def nodeViewSynchronizer(implicit system: ActorSystem): (ActorRef, PM, ConnectedPeer) = {
    val h = historyGen.sample.get
    val sRaw = stateGen.sample.get
    val v = h.openSurfaceIds().last

    val ncRef: ActorRef = ???
    val vhRef: ActorRef = ???
    val liRef: ActorRef = ???
    val sis: SIS = ???
    val ns: NetworkSettings = ???

    sRaw.store.update(ByteArrayWrapper(v), Seq(), Seq())
    val s = sRaw.copy(version = VersionTag @@ v)
    val ref = system.actorOf(NodeViewSynchronizerForTests.props(ncRef, vhRef, liRef, sis, ns))
    val m = totallyValidModifier(h, s)
    (ref, m, s, h)
  }
}