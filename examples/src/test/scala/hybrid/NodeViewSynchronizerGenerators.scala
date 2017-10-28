package hybrid

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import scorex.core.VersionTag
import scorex.core.network.ConnectedPeer
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageSpec
import scorex.core.settings.NetworkSettings
import examples.hybrid.history.HybridSyncInfoMessageSpec
import examples.hybrid.mining.HybridSettings
import scorex.testkit.generators.CoreGenerators


// todo: remove unused dependency injections
trait NodeViewSynchronizerGenerators {
  this: ModifierGenerators with StateGenerators with HistoryGenerators with HybridTypes with CoreGenerators =>

  object NodeViewSynchronizerForTests {
    // todo: there is nothing here that is special to tests. Should this `props` method be moved to NodeViewSynchronizer's companion object?
    def props(networkControllerRef: ActorRef,
              viewHolderRef: ActorRef,
              localInterfaceRef: ActorRef,
              syncInfoSpec: SIS,
              networkSettings: NetworkSettings): Props =
      Props(new NodeViewSynchronizer[P, TX, HSI, SIS](
        networkControllerRef, viewHolderRef, localInterfaceRef, syncInfoSpec, networkSettings))
  }

  def nodeViewSynchronizer(implicit system: ActorSystem):
  (ActorRef, PM, ConnectedPeer, TestProbe, TestProbe, TestProbe, TestProbe, MessageSpec[Serializable]) = {
    val h = historyGen.sample.get
    val sRaw = stateGen.sample.get
    val v = h.openSurfaceIds().last

    val ncProbe = TestProbe("NetworkControllerProbe")
    val vhProbe = TestProbe("ViewHolderProbe")
    val liProbe = TestProbe("LocalInterfaceProbe")
    val sis = HybridSyncInfoMessageSpec
    val settingsFilename: String = "examples/src/main/resources/settings.conf" // fixme: avoid magic constant
    val ns: NetworkSettings = HybridSettings.read(Some(settingsFilename)).scorexSettings.network

    sRaw.store.update(ByteArrayWrapper(v), Seq(), Seq())
    val s = sRaw.copy(version = VersionTag @@ v)
    val ref = system.actorOf(NodeViewSynchronizerForTests.props(ncProbe.ref, vhProbe.ref, liProbe.ref, sis, ns))
    val m = totallyValidModifier(h, s)

    val pchProbe = TestProbe("PeerHandlerProbe")

    val address: InetSocketAddress = inetSocketAddressGen.sample.get

    val p : ConnectedPeer = ConnectedPeer(address, pchProbe.ref)

    (ref, m, p, pchProbe, ncProbe, vhProbe, liProbe, sis)
  }
}