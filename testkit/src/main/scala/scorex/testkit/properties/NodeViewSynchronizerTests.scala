package scorex.testkit.properties

import akka.actor._
import akka.testkit.TestProbe
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import scorex.core.LocalInterface.{BetterNeighbourAppeared, NoBetterNeighbour}
import scorex.core.NodeViewHolder
import scorex.core.consensus.History.HistoryComparisonResult.{Equal, Nonsense, Older, Younger}
import scorex.core.network._
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Vault
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.utils.ScorexLogging
import scorex.core.{PersistentNodeViewModifier}
import scorex.testkit.generators.{SyntacticallyTargetedModifierProducer, TotallyValidModifierProducer}
import scorex.testkit.utils.{FileUtils, SequentialAkkaFixture}
import scorex.core.network.message._
import scorex.core.serialization.{BytesSerializable, Serializer}

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Failure

// todo: think about the following:
// with the current testing architecture, when a Scorex user (e.g. in "examples") wants to test his/her blockchain,
// he/she writes a NodeViewSynchronizerSpec extending NodeViewSynchronizerTests, and this will execute some tests
// that are actually independent of the particularities of his/her blockchain. Maybe we should test such
// blockchain-non-specific properties in scorex's core, instead of testkit.

// todo: remove unnecessary type parameters and traits
trait NodeViewSynchronizerTests[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier,
ST <: MinimalState[PM, ST],
SI <: SyncInfo,
HT <: History[PM, SI, HT],
MPool <: MemoryPool[TX, MPool],
VL <: Vault[P, TX, PM, VL]]
  extends SequentialAkkaFixture
    with Matchers
    with PropertyChecks
    with ScorexLogging
    with SyntacticallyTargetedModifierProducer[PM, SI, HT]
    with TotallyValidModifierProducer[PM, ST, SI, HT] {

  type Fixture = SynchronizerFixture

  def nodeViewSynchronizer(implicit system: ActorSystem): (ActorRef, SI, PM, TX, ConnectedPeer, TestProbe, TestProbe, TestProbe, TestProbe)

  class SynchronizerFixture extends AkkaFixture with FileUtils {
    val (node, syncInfo, mod, tx, peer, pchProbe, ncProbe, vhProbe, liProbe) = nodeViewSynchronizer
  }

  def createAkkaFixture(): Fixture = new SynchronizerFixture

  import NodeViewHolder._   // NodeViewHolder's messages
  import NodeViewSynchronizer._   // NodeViewSynchronizer's messages
  import NetworkController._      // NetworkController's messages

  property("NodeViewSynchronizer: SuccessfulTransaction") { ctx =>
    import ctx._
    node ! SuccessfulTransaction[P, TX](tx)
    ncProbe.fishForMessage(3 seconds) { case m => m.isInstanceOf[SendToNetwork] }
  }

  property("NodeViewSynchronizer: FailedTransaction") { ctx =>
    import ctx._
    node ! FailedTransaction[P, TX](tx, new Exception)
    // todo: NVS currently does nothing in this case. Should check banning.
  }

  property("NodeViewSynchronizer: SyntacticallySuccessfulModifier") { ctx =>
    import ctx._
    node ! SyntacticallySuccessfulModifier(mod)
    // todo ? : NVS currently does nothing in this case. Should it do?
  }

  property("NodeViewSynchronizer: SyntacticallyFailedModification") { ctx =>
    import ctx._
    node ! SyntacticallyFailedModification(mod, new Exception)
    // todo: NVS currently does nothing in this case. Should check banning.
  }

  property("NodeViewSynchronizer: SemanticallySuccessfulModifier") { ctx =>
    import ctx._
    node ! SemanticallySuccessfulModifier(mod)
    ncProbe.fishForMessage(3 seconds) { case m => m.isInstanceOf[SendToNetwork] }
  }

  property("NodeViewSynchronizer: SemanticallyFailedModification") { ctx =>
    import ctx._
    node ! SemanticallyFailedModification(mod, new Exception)
    // todo: NVS currently does nothing in this case. Should check banning.
  }

  property("NodeViewSynchronizer: GetLocalSyncInfo") { ctx =>
    import ctx._
    node ! GetLocalSyncInfo
    vhProbe.fishForMessage(3 seconds) { case m => m == GetSyncInfo }
  }

  property("NodeViewSynchronizer: CurrentSyncInfo") { ctx =>
    import ctx._
    node ! CurrentSyncInfo(syncInfo)
    ncProbe.fishForMessage(3 seconds) { case m =>
      m match {
        case SendToNetwork(Message (_, Right (info), None), SendToRandom) if info == syncInfo => true
        case _ => false
      }
    }
  }

  property("NodeViewSynchronizer: DataFromPeer: SyncInfoSpec") { ctx =>
    import ctx._

    val dummySyncInfoMessageSpec = new SyncInfoMessageSpec[SyncInfo](_ => Failure[SyncInfo](???)) { }

    val dummySyncInfo = new SyncInfo {
      def answer: Boolean = true
      def startingPoints: History.ModifierIds = Seq((mod.modifierTypeId, mod.id))
      type M = BytesSerializable
      def serializer: Serializer[M] = ???
    }

    node ! DataFromPeer(dummySyncInfoMessageSpec, dummySyncInfo, peer)
    vhProbe.fishForMessage(3 seconds) { case m => m == OtherNodeSyncingInfo(peer, dummySyncInfo) }
  }

  property("NodeViewSynchronizer: OtherNodeSyncingStatus: Nonsense") { ctx =>
    import ctx._
    node ! OtherNodeSyncingStatus(peer, Nonsense, syncInfo, syncInfo, None)
    // NVS does nothing in this case
  }

  property("NodeViewSynchronizer: OtherNodeSyncingStatus: Older") { ctx =>
    import ctx._
    node ! OtherNodeSyncingStatus(peer, Older, syncInfo, syncInfo, None)
    liProbe.fishForMessage(3 seconds) { case m => m == BetterNeighbourAppeared }
  }

  property("NodeViewSynchronizer: OtherNodeSyncingStatus: Older and then Younger") { ctx =>
    import ctx._
    node ! OtherNodeSyncingStatus(peer, Older, syncInfo, syncInfo, None)
    node ! OtherNodeSyncingStatus(peer, Younger, syncInfo, syncInfo, None)
    liProbe.fishForMessage(3 seconds) { case m => m == NoBetterNeighbour }
  }

  property("NodeViewSynchronizer: OtherNodeSyncingStatus: Younger with Non-Empty Extension") { ctx =>
    import ctx._
    node ! OtherNodeSyncingStatus(peer, Younger, syncInfo, syncInfo, Some(Seq((mod.modifierTypeId, mod.id))))
    ncProbe.fishForMessage(3 seconds) { case m =>
      m match {
        case SendToNetwork(Message(_, Right((tid, ids)), None), SendToPeer(p))
          if p == peer && tid == mod.modifierTypeId && ids == Seq(mod.id) => true
        case _ => false
      }
    }
  }

  property("NodeViewSynchronizer: OtherNodeSyncingStatus: Equal") { ctx =>
    import ctx._
    node ! OtherNodeSyncingStatus(peer, Equal, syncInfo, syncInfo, None)
    // NVS does nothing significant in this case
  }

  property("NodeViewSynchronizer: DataFromPeer: InvSpec") { ctx =>
    import ctx._
    val spec = new InvSpec(3)
    val modifiers = Seq(mod.id)
    node ! DataFromPeer(spec, (mod.modifierTypeId, modifiers), peer)
    vhProbe.fishForMessage(3 seconds) { case m => m == CompareViews(peer, mod.modifierTypeId, modifiers) }
  }

  property("NodeViewSynchronizer: DataFromPeer: RequestModifierSpec") { ctx =>
    import ctx._
    val spec = new RequestModifierSpec(3)
    val modifiers = Seq(mod.id)
    node ! DataFromPeer(spec, (mod.modifierTypeId, modifiers), peer)
    vhProbe.fishForMessage(3 seconds) { case m => m == GetLocalObjects(peer, mod.modifierTypeId, modifiers) }
  }

  property("NodeViewSynchronizer: DataFromPeer: Non-Asked Modifiers from Remote") { ctx =>
    import ctx._
    node ! DataFromPeer(ModifiersSpec, (mod.modifierTypeId, Map(mod.id -> mod.bytes)), peer)
    vhProbe.fishForMessage(3 seconds) { case m =>
      // Seq() because NodeViewSynchronizer ignores non-asked modifier
      m == ModifiersFromRemote(peer, mod.modifierTypeId, Seq())
    }
  }

  property("NodeViewSynchronizer: DataFromPeer: Asked Modifiers from Remote") { ctx =>
    import ctx._
    node ! RequestFromLocal(peer, mod.modifierTypeId, Seq(mod.id))
    node ! DataFromPeer(ModifiersSpec, (mod.modifierTypeId, Map(mod.id -> mod.bytes)), peer)
    vhProbe.fishForMessage(3 seconds) { case m =>
      m == ModifiersFromRemote(peer, mod.modifierTypeId, Seq(mod.bytes))
    }
  }

  property("NodeViewSynchronizer: RequestFromLocal") { ctx =>
    import ctx._
    node ! RequestFromLocal(peer, mod.modifierTypeId, Seq(mod.id))
    pchProbe.expectMsgType[Message[_]]
  }

  property("NodeViewSynchronizer: ResponseFromLocal") { ctx =>
    import ctx._
    node ! ResponseFromLocal(peer, mod.modifierTypeId, Seq(mod))
    pchProbe.expectMsgType[Message[_]]
  }

}
