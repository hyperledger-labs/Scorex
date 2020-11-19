package scorex.core.network

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.ObjectGenerators
import scorex.core.consensus.ContainsModifiers
import scorex.core.network.ModifiersStatus._
import scorex.core.serialization.ScorexSerializer
import scorex.core.{PersistentNodeViewModifier, ModifierTypeId}
import scorex.crypto.hash.Blake2b256
import scorex.util.{bytesToId, ModifierId}

import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

@SuppressWarnings(Array(
  "org.wartremover.warts.Null",
  "org.wartremover.warts.TraversableOps",
  "org.wartremover.warts.OptionPartial"))
class DeliveryTrackerSpecification extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with ObjectGenerators {

  val cp: ConnectedPeer = connectedPeerGen(null).sample.get
  val mtid: ModifierTypeId = ModifierTypeId @@ (0: Byte)

  class FakeModifier(val id: ModifierId) extends PersistentNodeViewModifier {
    override def parentId: ModifierId = ???

    override val modifierTypeId: ModifierTypeId = mtid
    override type M = this.type

    override def serializer: ScorexSerializer[FakeModifier.this.type] = ???
  }

  class FakeHistory extends ContainsModifiers[FakeModifier] {
    private val mods: TrieMap[ModifierId, FakeModifier] = TrieMap[ModifierId, FakeModifier]()

    override def modifierById(modifierId: ModifierId): Option[FakeModifier] = mods.get(modifierId)

    def put(mod: FakeModifier): Unit = mods.put(mod.id, mod)
  }

  property("expect from random") {
    val tracker = genDeliveryTracker

    val modids: Seq[ModifierId] = Seq(Blake2b256("1"), Blake2b256("2"), Blake2b256("3")).map(bytesToId)

    tracker.setRequested(modids, mtid, None)
    modids.foreach(id => tracker.status(id) shouldBe Requested)

    // reexpect
    tracker.setRequested(modids, mtid, None)
    modids.foreach(id => tracker.status(id) shouldBe Requested)
  }

  property("locally generated modifier") {
    val tracker = genDeliveryTracker
    val history = new FakeHistory

    val modids: Seq[ModifierId] = Seq(Blake2b256("1"), Blake2b256("2"), Blake2b256("3")).map(bytesToId)

    modids.foreach(id => history.put(new FakeModifier(id)))
    modids.foreach(id => tracker.setHeld(id))
    modids.foreach(id => tracker.status(id, history) shouldBe Held)
  }

  property("persistent modifier workflow") {
    val tracker = genDeliveryTracker
    val history = new FakeHistory

    val modids: Seq[ModifierId] = Seq(Blake2b256("1"), Blake2b256("2"), Blake2b256("3")).map(bytesToId)

    modids.foreach(id => tracker.status(id) shouldBe Unknown)
    modids.foreach(id => tracker.status(id) should not be Requested)

    tracker.setRequested(modids, mtid, Some(cp))
    modids.foreach(id => tracker.status(id) shouldBe Requested)
    modids.foreach(id => tracker.status(id) shouldBe Requested)

    // received correct modifier
    val received = modids.head
    tracker.status(received) shouldBe Requested
    tracker.setReceived(received, cp)
    tracker.status(received) shouldBe Received
    tracker.status(received) should not be Requested

    history.put(new FakeModifier(received))
    tracker.setHeld(received)
    tracker.status(received, history) shouldBe Held

    // received incorrect modifier
    val invalid = modids.last
    tracker.status(invalid) shouldBe Requested
    tracker.setReceived(invalid, cp)
    tracker.status(invalid) shouldBe Received
    tracker.setInvalid(invalid)
    tracker.status(invalid, history) shouldBe Invalid
    tracker.status(invalid) should not be Requested

    // modifier was not delivered on time
    val nonDelivered = modids(1)
    tracker.status(nonDelivered) shouldBe Requested
    tracker.setUnknown(nonDelivered)
    tracker.status(nonDelivered, history) shouldBe Unknown
    tracker.status(nonDelivered) should not be Requested
  }

  property("stop expecting after maximum number of retries") {
    val tracker = genDeliveryTracker

    val modids: Seq[ModifierId] = Seq(Blake2b256("1"), Blake2b256("2"), Blake2b256("3")).map(bytesToId)

    tracker.setRequested(modids, mtid, Some(cp))
    tracker.status(modids.head) shouldBe Requested

    tracker.setReceived(modids.head, cp)

    tracker.status(modids.head) should not be Requested


    tracker.onStillWaiting(cp, mtid, modids(1))
    tracker.status(modids(1)) shouldBe Requested

    tracker.onStillWaiting(cp, mtid, modids(1))
    tracker.status(modids(1)) should not be Requested
  }

  private def genDeliveryTracker = {
    val system = ActorSystem()
    val probe = TestProbe("p")(system)
    implicit val nvsStub: ActorRef = probe.testActor
    val dt = FiniteDuration(3, MINUTES)
    new DeliveryTracker(system, deliveryTimeout = dt, maxDeliveryChecks = 2, nvsStub)
  }

}
