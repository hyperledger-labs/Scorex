package scorex.core.network

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.ObjectGenerators
import scorex.core.consensus.ModifierContaining
import scorex.core.network.ModifiersStatus.ModifiersStatus
import scorex.core.serialization.Serializer
import scorex.core.{ModifierId, ModifierTypeId, PersistentNodeViewModifier, bytesToId}
import scorex.crypto.hash.Blake2b256

import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

@SuppressWarnings(Array(
  "org.wartremover.warts.Null",
  "org.wartremover.warts.TraversableOps",
  "org.wartremover.warts.OptionPartial"))
class DeliveryTrackerSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ObjectGenerators {

  class FakeModifier(val id: ModifierId) extends PersistentNodeViewModifier {
    override def parentId: ModifierId = ???
    override val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (0: Byte)
    override type M = this.type
    override def serializer: Serializer[FakeModifier.this.type] = ???
  }

  class FakeHistory extends ModifierContaining[FakeModifier] {
    private val mods: TrieMap[ModifierId, FakeModifier] = TrieMap[ModifierId, FakeModifier]()

    override def modifierById(modifierId: ModifierId): Option[FakeModifier] = mods.get(modifierId)

    def put(mod: FakeModifier): Unit = mods.put(mod.id, mod)
  }

  property("expect from random") {
    implicit val system = ActorSystem()
    val probe = TestProbe("p")(system)
    implicit val nvsStub: ActorRef = probe.testActor

    val dt = FiniteDuration(3, MINUTES)

    val tracker = new DeliveryTracker(system, deliveryTimeout = dt, maxDeliveryChecks = 2, nvsStub)

    val mtid = ModifierTypeId @@ (0: Byte)
    val modids: Seq[ModifierId] = Seq(Blake2b256("1"), Blake2b256("2"), Blake2b256("3")).map(bytesToId)

    tracker.onRequest(None, mtid, modids)
    modids.foreach(id => tracker.isExpecting(id) shouldBe true)

    // reexpect
    tracker.onRequest(None, mtid, modids)
    modids.foreach(id => tracker.isExpecting(id) shouldBe true)
  }

  property("Normal workflow") {
    implicit val system = ActorSystem()
    val probe = TestProbe("p")(system)
    implicit val nvsStub: ActorRef = probe.testActor
    val dt = FiniteDuration(3, MINUTES)
    val tracker = new DeliveryTracker(system, deliveryTimeout = dt, maxDeliveryChecks = 2, nvsStub)
    val cp = ConnectedPeer(new InetSocketAddress(55), null, null, null)
    val mtid = ModifierTypeId @@ (0: Byte)
    val history = new FakeHistory

    val modids: Seq[ModifierId] = Seq(Blake2b256("1"), Blake2b256("2"), Blake2b256("3")).map(bytesToId)

    modids.foreach(id => tracker.status(id) shouldBe ModifiersStatus.Unknown)

    tracker.onRequest(Some(cp), mtid, modids)
    modids.foreach(id => tracker.status(id) shouldBe ModifiersStatus.Requested)

    // received correct modifier
    val received = modids.head
    tracker.onReceive(received)
    tracker.status(received) shouldBe ModifiersStatus.Received

    history.put(new FakeModifier(received))
    tracker.onApply(received)
    tracker.status(received, history) shouldBe ModifiersStatus.Applied

    // received incorrect modifier
    val invalid = modids.last
    tracker.onReceive(invalid)
    tracker.status(invalid) shouldBe ModifiersStatus.Received
    tracker.onInvalid(invalid)
    tracker.status(invalid, history) shouldBe ModifiersStatus.Invalid

    // modifier was not delivered on time
    val nonDelivered = modids(1)
    tracker.stopProcessing(nonDelivered)
    tracker.status(nonDelivered, history) shouldBe ModifiersStatus.Unknown

  }

  property("basic ops") {
    implicit val system = ActorSystem()
    val probe = TestProbe("p")(system)
    implicit val nvsStub: ActorRef = probe.testActor

    val dt = FiniteDuration(3, MINUTES)

    val tracker = new DeliveryTracker(system, deliveryTimeout = dt, maxDeliveryChecks = 2, nvsStub)

    val cp = ConnectedPeer(new InetSocketAddress(55), null, null, null)

    val mtid = ModifierTypeId @@ (0: Byte)

    val modids: Seq[ModifierId] = Seq(Blake2b256("1"), Blake2b256("2"), Blake2b256("3")).map(bytesToId)

    val notAdded: ModifierId = bytesToId(Blake2b256("4"))

    tracker.onRequest(Some(cp), mtid, modids)
    tracker.isExpecting(modids.head) shouldBe true

    tracker.isExpecting(notAdded) shouldBe false

    tracker.onReceive(modids.head)

    tracker.isExpecting(modids.head) shouldBe false

    tracker.onReceive(notAdded) shouldBe false

    tracker.reexpect(Some(cp), mtid, modids(1))
    tracker.isExpecting(modids(1)) shouldBe true

    tracker.reexpect(Some(cp), mtid, modids(1))
    tracker.isExpecting(modids(1)) shouldBe false

    tracker.reexpect(Some(cp), mtid, modids(1))
    tracker.isExpecting(modids(1)) shouldBe true

    tracker.reexpect(Some(cp), mtid, modids.head)
    tracker.isExpecting(modids.head) shouldBe true
  }
}