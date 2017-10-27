package hybrid

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import scorex.core.VersionTag
import scorex.core.network.ConnectedPeer
import scorex.core.network.NodeViewSynchronizer
import scorex.core.settings.NetworkSettings
import scorex.core.utils.ScorexLogging
import examples.hybrid.history.HybridSyncInfoMessageSpec
import examples.hybrid.mining.HybridSettings


// todo: remove unused dependency injections
trait NodeViewSynchronizerGenerators { this: ModifierGenerators with StateGenerators with HistoryGenerators with HybridTypes =>

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


  // fixme: the following lines were copy-pasted from "src/test/scala/scorex/ObjectGenerators.scala" because it cannot be imported here.
  private val MaxIp = 255
  private val MaxPort = 65535
  private lazy val inetSocketAddressGen = for {
    ip1 <- Gen.choose(0, MaxIp)
    ip2 <- Gen.choose(0, MaxIp)
    ip3 <- Gen.choose(0, MaxIp)
    ip4 <- Gen.choose(0, MaxIp)
    port <- Gen.choose(0, MaxPort)
  } yield new InetSocketAddress(InetAddress.getByName(s"$ip1.$ip2.$ip3.$ip4"), port)


  def nodeViewSynchronizer(implicit system: ActorSystem): (ActorRef, PM, ConnectedPeer, TestProbe, TestProbe, TestProbe, TestProbe) = {
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

    (ref, m, p, pchProbe, ncProbe, vhProbe, liProbe)
  }
}