package scorex.testkit.properties

import akka.actor._
import akka.testkit.TestProbe
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import scorex.core.network.{ConnectedPeer, NetworkController, NodeViewSynchronizer}
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Vault
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.utils.ScorexLogging
import scorex.core.PersistentNodeViewModifier
import scorex.testkit.generators.{SyntacticallyTargetedModifierProducer, TotallyValidModifierProducer}
import scorex.testkit.utils.{FileUtils, SequentialAkkaFixture}
import scorex.core.network.message._

import scala.concurrent.duration._
import scala.language.postfixOps

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

  def nodeViewSynchronizer(implicit system: ActorSystem): (ActorRef, PM, ConnectedPeer, TestProbe, TestProbe, TestProbe, TestProbe, MessageSpec[Serializable])

  class SynchronizerFixture extends AkkaFixture with FileUtils {
    val (node, mod, peer, pchProbe, ncProbe, vhProbe, liProbe, syncInfoMessageSpec) = nodeViewSynchronizer
  }

  def createAkkaFixture(): Fixture = new SynchronizerFixture

  import NodeViewSynchronizer._   // NodeViewSynchronizer's messages
  import NetworkController._      // NetworkController's messages

  property("NodeViewSynchronizer: SuccessfulTransaction") { ctx =>
    import ctx._

  }

  property("NodeViewSynchronizer: FailedTransaction") { ctx =>
    import ctx._

  }

  property("NodeViewSynchronizer: SyntacticallySuccessfulModifier") { ctx =>
    import ctx._

  }

  property("NodeViewSynchronizer: SyntacticallyFailedModification") { ctx =>
    import ctx._

  }

  property("NodeViewSynchronizer: SemanticallySuccessfulModifier") { ctx =>
    import ctx._

  }

  property("NodeViewSynchronizer: SemanticallyFailedModification") { ctx =>
    import ctx._

  }

  property("NodeViewSynchronizer: GetLocalSyncInfo") { ctx =>
    import ctx._

  }

  property("NodeViewSynchronizer: CurrentSyncInfo") { ctx =>
    import ctx._

  }

  property("NodeViewSynchronizer: DataFromPeer: SyncInfoSpec") { ctx =>
    import ctx._
    val modifiers = Seq(mod.id)
    node ! DataFromPeer(syncInfoMessageSpec, (mod.modifierTypeId, modifiers), peer)
    vhProbe.fishForMessage(3 seconds) { case m => m == CompareViews(peer, mod.modifierTypeId, modifiers) }
  }

  property("NodeViewSynchronizer: OtherNodeSyncingStatus") { ctx =>
    import ctx._

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

  property("NodeViewSynchronizer: DataFromPeer: ModifiersSpec") { ctx =>
    import ctx._
    // todo
//    val spec = ModifiersSpec // fixme
//    val modifiers = Seq(mod.id)
//    node ! DataFromPeer(spec, (mod.modifierTypeId, modifiers), peer) // fixme
//    vhProbe.fishForMessage(3 seconds) { case m => m == ModifiersFromRemote(peer, mod.modifierTypeId, modifiers) }
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


  // todo: check that source is added to `added`

}
