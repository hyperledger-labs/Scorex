package scorex.core.network

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.ObjectGenerators
import scorex.core.consensus.ModifierContaining
import scorex.core.network.ModifiersStatus._
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

  val cp: ConnectedPeer = connectedPeerGen(null).sample.get
  val mtid: ModifierTypeId = ModifierTypeId @@ (0: Byte)

  class FakeModifier(val id: ModifierId) extends PersistentNodeViewModifier {
    override def parentId: ModifierId = ???

    override val modifierTypeId: ModifierTypeId = mtid
    override type M = this.type

    override def serializer: Serializer[FakeModifier.this.type] = ???
  }

  class FakeHistory extends ModifierContaining[FakeModifier] {
    private val mods: TrieMap[ModifierId, FakeModifier] = TrieMap[ModifierId, FakeModifier]()

    override def modifierById(modifierId: ModifierId): Option[FakeModifier] = mods.get(modifierId)

    def put(mod: FakeModifier): Unit = mods.put(mod.id, mod)
  }

  property("expect from random") {
    val tracker = genDeliveryTracker

    val modids: Seq[ModifierId] = Seq(Blake2b256("1"), Blake2b256("2"), Blake2b256("3")).map(bytesToId)

    tracker.onRequest(None, mtid, modids)
    modids.foreach(id => tracker.isExpecting(id) shouldBe true)

    // reexpect
    tracker.onRequest(None, mtid, modids)
    modids.foreach(id => tracker.isExpecting(id) shouldBe true)
  }

  property("locally generated modifier") {
    val tracker = genDeliveryTracker
    val history = new FakeHistory

    val modids: Seq[ModifierId] = Seq(Blake2b256("1"), Blake2b256("2"), Blake2b256("3")).map(bytesToId)

    modids.foreach(id => history.put(new FakeModifier(id)))
    modids.foreach(id => tracker.onApply(id))
    modids.foreach(id => tracker.status(id, history) shouldBe Applied)
  }

  property("persistent modifier workflow") {
    val tracker = genDeliveryTracker
    val history = new FakeHistory

    val modids: Seq[ModifierId] = Seq(Blake2b256("1"), Blake2b256("2"), Blake2b256("3")).map(bytesToId)

    modids.foreach(id => tracker.status(id) shouldBe Unknown)
    modids.foreach(id => tracker.isExpecting(id) shouldBe false)

    tracker.onRequest(Some(cp), mtid, modids)
    modids.foreach(id => tracker.status(id) shouldBe Requested)
    modids.foreach(id => tracker.isExpecting(id) shouldBe true)

    // received correct modifier
    val received = modids.head
    tracker.isExpecting(received) shouldBe true
    tracker.onReceive(received)
    tracker.status(received) shouldBe Received
    tracker.isExpecting(received) shouldBe false

    history.put(new FakeModifier(received))
    tracker.onApply(received)
    tracker.status(received, history) shouldBe Applied

    // received incorrect modifier
    val invalid = modids.last
    tracker.isExpecting(invalid) shouldBe true
    tracker.onReceive(invalid)
    tracker.status(invalid) shouldBe Received
    tracker.onInvalid(invalid)
    tracker.status(invalid, history) shouldBe Invalid
    tracker.isExpecting(invalid) shouldBe false

    // modifier was not delivered on time
    val nonDelivered = modids(1)
    tracker.isExpecting(nonDelivered) shouldBe true
    tracker.stopProcessing(nonDelivered)
    tracker.status(nonDelivered, history) shouldBe Unknown
    tracker.isExpecting(nonDelivered) shouldBe false

  }

  property("Spam attempt") {
    val tracker = genDeliveryTracker
    val notAdded: ModifierId = bytesToId(Blake2b256("4"))
    tracker.isExpecting(notAdded) shouldBe false
    tracker.onReceive(notAdded) shouldBe false
    tracker.status(notAdded) shouldBe Unknown
  }

  property("stop expecting after maximum number of retries") {
    val tracker = genDeliveryTracker

    val modids: Seq[ModifierId] = Seq(Blake2b256("1"), Blake2b256("2"), Blake2b256("3")).map(bytesToId)

    tracker.onRequest(Some(cp), mtid, modids)
    tracker.isExpecting(modids.head) shouldBe true

    tracker.onReceive(modids.head)

    tracker.isExpecting(modids.head) shouldBe false


    tracker.onStillWaiting(cp, mtid, modids(1))
    tracker.isExpecting(modids(1)) shouldBe true

    tracker.onStillWaiting(cp, mtid, modids(1))
    tracker.isExpecting(modids(1)) shouldBe false
  }

  private def genDeliveryTracker = {
    val system = ActorSystem()
    val probe = TestProbe("p")(system)
    implicit val nvsStub: ActorRef = probe.testActor
    val dt = FiniteDuration(3, MINUTES)
    new DeliveryTracker(system, deliveryTimeout = dt, maxDeliveryChecks = 2, nvsStub)
  }

}