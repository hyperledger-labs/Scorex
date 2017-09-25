package scorex.testkit.properties

import akka.actor._
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.EventType.{FailedPersistentModifier, SuccessfulPersistentModifier}
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Vault
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.utils.ScorexLogging
import scorex.core.{NodeViewHolder, PersistentNodeViewModifier}
import scorex.testkit.utils.{FileUtils, SequentialAkkaFixture}

trait NodeViewHolderTests[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier,
ST <: MinimalState[PM,ST],
SI <: SyncInfo,
HT <: History[PM, SI, HT],
MPool <: MemoryPool[TX, MPool],
VL <: Vault[P, TX, PM, VL]]
  extends SequentialAkkaFixture
    with Matchers
    with PropertyChecks
    with ScorexLogging {

  type Fixture = HolderFixture

  def nodeViewHolder(implicit system: ActorSystem): (ActorRef, PM, ST, HT)

  class HolderFixture extends AkkaFixture with FileUtils {
    val (node, mod, s, h) = nodeViewHolder
  }

  def createAkkaFixture(): Fixture = new HolderFixture

  import NodeViewHolder._

  property("NodeViewHolder: check state after creation") { ctx => import ctx._
    node ! GetDataFromCurrentView[HT, ST, VL, MPool, Boolean] { v =>
      v.state.version.sameElements(s.version)
    }
    expectMsg(true)
  }

  property("NodeViewHolder: check valid modifier is applicable") { ctx => import ctx._
    node ! GetDataFromCurrentView[HT, ST, VL, MPool, Boolean] { v =>
      v.history.applicable(mod)
    }
    expectMsg(true)
  }

  property("NodeViewHolder: check sync info is synced") { ctx => import ctx._
    node ! GetSyncInfo
    val syncInfo = CurrentSyncInfo(h.syncInfo(false))
    expectMsg(syncInfo)
  }

  property("NodeViewHolder: apply locally generated mod") { ctx => import ctx._
    node ! NodeViewHolder.Subscribe(Seq(SuccessfulPersistentModifier, FailedPersistentModifier))
    node ! LocallyGeneratedModifier(mod)
    node ! GetDataFromCurrentView[HT, ST, VL, MPool, Boolean] { v =>
      v.state.version.sameElements(s.version) && v.history.contains(mod.id)
    }

    fishForMessage() {
      case x: Boolean => x
      case _ => false
    }
  }
}
