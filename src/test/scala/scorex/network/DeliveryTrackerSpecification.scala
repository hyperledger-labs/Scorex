package scorex.core.network

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.ObjectGenerators
import scorex.core.{ModifierId, ModifierTypeId, bytesToId}
import scorex.crypto.hash.Blake2b256

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

  property("expect from random") {
    implicit val system = ActorSystem()
    val probe = TestProbe("p")(system)
    implicit val nvsStub: ActorRef = probe.testActor

    val dt = FiniteDuration(3, MINUTES)

    val tracker = new DeliveryTracker(system, deliveryTimeout = dt, maxDeliveryChecks = 2, nvsStub)

    val mtid = ModifierTypeId @@ (0: Byte)
    val modids: Seq[ModifierId] = Seq(Blake2b256("1"), Blake2b256("2"), Blake2b256("3")).map(bytesToId)

    tracker.expect(mtid, modids)
    modids.foreach(id => tracker.isExpecting(id) shouldBe true)

    // reexpect
    tracker.expect(mtid, modids)
    modids.foreach(id => tracker.isExpecting(id) shouldBe true)
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

    tracker.expect(cp, mtid, modids)

    tracker.isExpecting(modids.head) shouldBe true

    tracker.isExpecting(notAdded) shouldBe false

    tracker.onReceive(mtid, modids.head, cp)

    tracker.isExpecting(modids.head) shouldBe false

    tracker.onReceive(mtid, notAdded, cp) shouldBe false

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