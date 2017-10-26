package scorex.testkit.properties

import akka.actor._
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.network.{ConnectedPeer, NodeViewSynchronizer}
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Vault
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.utils.ScorexLogging
import scorex.core.{ModifierId, ModifierTypeId, PersistentNodeViewModifier}
import scorex.testkit.generators.{SyntacticallyTargetedModifierProducer, TotallyValidModifierProducer}
import scorex.testkit.utils.{FileUtils, SequentialAkkaFixture}

import scala.concurrent.duration._

// todo: remove unnecessary type parameters

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

  def nodeViewSynchronizer(implicit system: ActorSystem): (ActorRef, PM, ConnectedPeer)

  class SynchronizerFixture extends AkkaFixture with FileUtils {
    val (node, mod, peer) = nodeViewSynchronizer
  }

  def createAkkaFixture(): Fixture = new SynchronizerFixture

  import NodeViewSynchronizer._

  property("NodeViewSynchronizer: request from local") { ctx =>
    import ctx._

    node ! RequestFromLocal(peer, mod.modifierTypeId, Seq(mod.id))
    // todo: check that source is added to `added`
    expectMsg(true)
  }

}
