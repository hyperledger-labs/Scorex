package scorex.testkit.properties

import akka.actor._
import akka.testkit.TestProbe
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.core.NodeViewHolder.ReceivableMessages.{ChangedCache, GetNodeViewChanges}
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.History.{Equal, Nonsense, Older, Younger}
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.network.NetworkController.ReceivableMessages.{Blacklist, SendToNetwork}
import scorex.core.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
import scorex.core.network.NodeViewSynchronizer.Events.{BetterNeighbourAppeared, NoBetterNeighbour, NodeViewSynchronizerEvent}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
import scorex.core.network._
import scorex.core.network.message._
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.utils.ScorexLogging
import scorex.testkit.generators.{SyntacticallyTargetedModifierProducer, TotallyValidModifierProducer}
import scorex.testkit.utils.AkkaFixture

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Failure

@SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
trait NodeViewSynchronizerTests[
  TX <: Transaction,
  PM <: PersistentNodeViewModifier,
  ST <: MinimalState[PM, ST],
  SI <: SyncInfo,
  HT <: History[PM, SI, HT],
  MP <: MemoryPool[TX, MP]
] extends PropSpec
    with Matchers
    with PropertyChecks
    with ScorexLogging
    with SyntacticallyTargetedModifierProducer[PM, SI, HT]
    with TotallyValidModifierProducer[PM, ST, SI, HT] {

  val historyGen: Gen[HT]
  val memPool: MP

  def nodeViewSynchronizer(implicit system: ActorSystem): (ActorRef, SI, PM, TX, ConnectedPeer, TestProbe, TestProbe, TestProbe, TestProbe)

  class SynchronizerFixture extends AkkaFixture {
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val (node, syncInfo, mod, tx, peer, pchProbe, ncProbe, vhProbe, eventListener) = nodeViewSynchronizer
  }

  // ToDo: factor this out of here and NVHTests?
  private def withFixture(testCode: SynchronizerFixture => Any): Unit = {
    val fixture = new SynchronizerFixture
    try {
      testCode(fixture)
    }
    finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }


  property("NodeViewSynchronizer: SuccessfulTransaction") {
    withFixture { ctx =>
      import ctx._
      node ! SuccessfulTransaction[TX](tx)
      ncProbe.fishForMessage(3 seconds) { case m => m.isInstanceOf[SendToNetwork] }
    }
  }

  property("NodeViewSynchronizer: FailedTransaction") {
    withFixture { ctx =>
      import ctx._
      node ! FailedTransaction[TX](tx, new Exception)
      // todo: NVS currently does nothing in this case. Should check banning.
    }
  }

  property("NodeViewSynchronizer: SyntacticallySuccessfulModifier") {
    withFixture { ctx =>
      import ctx._
      node ! SyntacticallySuccessfulModifier(mod)
      // todo ? : NVS currently does nothing in this case. Should it do?
    }
  }

  property("NodeViewSynchronizer: SyntacticallyFailedModification") {
    withFixture { ctx =>
      import ctx._
      node ! SyntacticallyFailedModification(mod, new Exception)
      // todo: NVS currently does nothing in this case. Should check banning.
    }
  }

  property("NodeViewSynchronizer: SemanticallySuccessfulModifier") {
    withFixture { ctx =>
      import ctx._
      node ! SemanticallySuccessfulModifier(mod)
      ncProbe.fishForMessage(3 seconds) { case m => m.isInstanceOf[SendToNetwork] }
    }
  }

  property("NodeViewSynchronizer: SemanticallyFailedModification") {
    withFixture { ctx =>
      import ctx._
      node ! SemanticallyFailedModification(mod, new Exception)
      // todo: NVS currently does nothing in this case. Should check banning.
    }
  }

  //TODO rewrite
  ignore("NodeViewSynchronizer: DataFromPeer: SyncInfoSpec") {
    withFixture { ctx =>
      import ctx._

      val dummySyncInfoMessageSpec = new SyncInfoMessageSpec[SyncInfo](_ => Failure[SyncInfo](new Exception)) {}

      val dummySyncInfo = new SyncInfo {
        def answer: Boolean = true

        def startingPoints: History.ModifierIds = Seq((mod.modifierTypeId, mod.id))

        type M = BytesSerializable

        def serializer: Serializer[M] = throw new Exception
      }

      node ! DataFromPeer(dummySyncInfoMessageSpec, dummySyncInfo, peer)
      //    vhProbe.fishForMessage(3 seconds) { case m => m == OtherNodeSyncingInfo(peer, dummySyncInfo) }
    }
  }

  property("NodeViewSynchronizer: OtherNodeSyncingStatus: Nonsense") {
    withFixture { ctx =>
      import ctx._
      node ! OtherNodeSyncingStatus(peer, Nonsense, None)
      // NVS does nothing in this case
    }
  }

  property("NodeViewSynchronizer: OtherNodeSyncingStatus: Older") {
    withFixture { ctx =>
      import ctx._
      system.eventStream.subscribe(eventListener.ref, classOf[NodeViewSynchronizerEvent])
      node ! OtherNodeSyncingStatus(peer, Older, None)
      eventListener.fishForMessage(3 seconds) { case m => m == BetterNeighbourAppeared }
    }
  }

  property("NodeViewSynchronizer: OtherNodeSyncingStatus: Older and then Younger") {
    withFixture { ctx =>
      import ctx._
      system.eventStream.subscribe(eventListener.ref, classOf[NodeViewSynchronizerEvent])
      node ! OtherNodeSyncingStatus(peer, Older, None)
      node ! OtherNodeSyncingStatus(peer, Younger, None)
      eventListener.fishForMessage(3 seconds) { case m => m == NoBetterNeighbour }
    }
  }

  property("NodeViewSynchronizer: OtherNodeSyncingStatus: Younger with Non-Empty Extension") {
    withFixture { ctx =>
      import ctx._
      node ! OtherNodeSyncingStatus(peer, Younger, Some(Seq((mod.modifierTypeId, mod.id))))
      ncProbe.fishForMessage(3 seconds) { case m =>
        m match {
          case SendToNetwork(Message(_, Right((tid, ids)), None), SendToPeer(p))
            if p == peer && tid == mod.modifierTypeId && ids == Seq(mod.id) => true
          case _ => false
        }
      }
    }
  }

  property("NodeViewSynchronizer: OtherNodeSyncingStatus: Equal") {
    withFixture { ctx =>
      import ctx._
      node ! OtherNodeSyncingStatus(peer, Equal, None)
      // NVS does nothing significant in this case
    }
  }

  property("NodeViewSynchronizer: DataFromPeer: InvSpec") {
    withFixture { ctx =>
      import ctx._
      val spec = new InvSpec(3)
      val modifiers = Seq(mod.id)
      node ! DataFromPeer(spec, (mod.modifierTypeId, modifiers), peer)
      pchProbe.fishForMessage(5 seconds) {
        case _: Message[_] => true
        case _ => false
      }
    }
  }

  property("NodeViewSynchronizer: DataFromPeer: RequestModifierSpec") {
    withFixture { ctx =>
      import ctx._
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val h = historyGen.sample.get
      val mod = syntacticallyValidModifier(h)
      val (newH, _) = h.append(mod).get
      val m = memPool
      val spec = new RequestModifierSpec(3)
      val modifiers = Seq(mod.id)
      node ! ChangedHistory(newH)
      node ! ChangedMempool(m)
      node ! DataFromPeer(spec, (mod.modifierTypeId, modifiers), peer)

      pchProbe.fishForMessage(5 seconds) {
        case _: Message[_] => true
        case _ => false
      }
    }
  }

  ignore("NodeViewSynchronizer: DataFromPeer: Non-Asked Modifiers from Remote") {
    withFixture { ctx =>
      import ctx._

      val modifiersSpec = new ModifiersSpec(1024 * 1024)

      node ! DataFromPeer(modifiersSpec, (mod.modifierTypeId, Map(mod.id -> mod.bytes)), peer)
      // todo complete at least after blacklist implementation
    }
  }

  property("NodeViewSynchronizer: DataFromPeer: Asked Modifiers from Remote") {
    withFixture { ctx =>
      import ctx._
      vhProbe.expectMsgType[GetNodeViewChanges]

      val modifiersSpec = new ModifiersSpec(1024 * 1024)

      node ! DataFromPeer(new InvSpec(3), (mod.modifierTypeId, Seq(mod.id)), peer)
      node ! DataFromPeer(modifiersSpec, (mod.modifierTypeId, Map(mod.id -> mod.bytes)), peer)
      vhProbe.expectMsgType[ChangedCache[_, _, _]]
    }
  }

  property("NodeViewSynchronizer: DataFromPeer - CheckDelivery -  Do not penalize if delivered") {
    withFixture { ctx =>
      import ctx._

      val modifiersSpec = new ModifiersSpec(1024 * 1024)

      node ! DataFromPeer(new InvSpec(3), (mod.modifierTypeId, Seq(mod.id)), peer)
      node ! DataFromPeer(modifiersSpec, (mod.modifierTypeId, Map(mod.id -> mod.bytes)), peer)
      system.scheduler.scheduleOnce(1 second, node, DataFromPeer(modifiersSpec, (mod.modifierTypeId, Map(mod.id -> mod.bytes)), peer))
      val messages = ncProbe.receiveWhile(max = 5 seconds, idle = 1 second) { case m => m }
      assert(!messages.contains(Blacklist(peer)))
    }
  }

  property("NodeViewSynchronizer: ResponseFromLocal") {
    withFixture { ctx =>
      import ctx._
      node ! ResponseFromLocal(peer, mod.modifierTypeId, Seq(mod))
      pchProbe.expectMsgType[Message[_]]
    }
  }

}
