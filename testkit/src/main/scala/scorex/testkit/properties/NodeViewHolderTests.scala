package scorex.testkit.properties

import akka.actor._
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.EventType._
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Vault
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.utils.ScorexLogging
import scorex.core.{NodeViewHolder, PersistentNodeViewModifier}
import scorex.testkit.generators.{SemanticallyInvalidModifierProducer, SyntacticallyTargetedModifierProducer, TotallyValidModifierProducer}
import scorex.testkit.utils.{FileUtils, SequentialAkkaFixture}

import scala.concurrent.duration._

import scala.language.postfixOps


trait NodeViewHolderTests[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier,
ST <: MinimalState[PM, ST],
SI <: SyncInfo,
HT <: History[PM, SI, HT],
MPool <: MemoryPool[TX, MPool]]
  extends SequentialAkkaFixture
    with Matchers
    with PropertyChecks
    with ScorexLogging
    with SyntacticallyTargetedModifierProducer[PM, SI, HT]
    with TotallyValidModifierProducer[PM, ST, SI, HT]
    with SemanticallyInvalidModifierProducer[PM, ST] {

  type Fixture = HolderFixture
  //type VL <: Vault[P, TX, PM, VL]

  def nodeViewHolder(implicit system: ActorSystem): (ActorRef, PM, ST, HT)

  class HolderFixture extends AkkaFixture with FileUtils {
    val (node, mod, s, h) = nodeViewHolder
  }

  def createAkkaFixture(): Fixture = new HolderFixture

  val semantic = Seq(SuccessfulSemanticallyValidModifier, SemanticallyFailedPersistentModifier)
  val syntactic = Seq(SuccessfulSyntacticallyValidModifier, SyntacticallyFailedPersistentModifier)
  val allEvents = semantic ++ syntactic

  import NodeViewHolder._


  property("NodeViewHolder: syntactically valid modifier subscription") { ctx =>
    import ctx._

    node ! NodeViewHolder.Subscribe(syntactic)
    node ! GetDataFromCurrentView[HT, ST, _ <: Vault[P, TX, PM, _], MPool, PM] { v => totallyValidModifiers(v.history, v.state, 2).head }
    val mod = receiveOne(5 seconds).asInstanceOf[PM]
    node ! LocallyGeneratedModifier(mod)
    expectMsgType[SyntacticallySuccessfulModifier[PM]]

  }

  property("NodeViewHolder: syntactically failed modifier subscription") { ctx =>
    import ctx._

    node ! NodeViewHolder.Subscribe(syntactic)
    val invalid = syntacticallyInvalidModifier(h)
    node ! LocallyGeneratedModifier(invalid)
    expectMsgType[SyntacticallyFailedModification[PM]]
  }

  property("NodeViewHolder: semantically valid modifier subscription") { ctx =>
    import ctx._

    node ! NodeViewHolder.Subscribe(allEvents)
    node ! GetDataFromCurrentView[HT, ST, _, MPool, PM] { v => totallyValidModifiers(v.history, v.state, 2).head }
    val mod = receiveOne(5 seconds).asInstanceOf[PM]
    node ! LocallyGeneratedModifier(mod)
    expectMsgType[SyntacticallySuccessfulModifier[PM]]
    expectMsgType[SemanticallySuccessfulModifier[PM]]
  }

  property("NodeViewHolder: semantically failed modifier subscription") { ctx =>
    import ctx._

    node ! NodeViewHolder.Subscribe(allEvents)
    node ! GetDataFromCurrentView[HT, ST, _, MPool, PM] { v => semanticallyInvalidModifier(v.state) }
    val invalid = receiveOne(5 seconds).asInstanceOf[PM]
    node ! LocallyGeneratedModifier(invalid)
    expectMsgType[SyntacticallySuccessfulModifier[PM]]
    expectMsgType[SemanticallyFailedModification[PM]]
  }

  property("NodeViewHolder: syntactically/semantically valid modifier subscription") { ctx =>
    import ctx._

    node ! NodeViewHolder.Subscribe(allEvents)

    node ! GetDataFromCurrentView[HT, ST, _, MPool, PM] { v => totallyValidModifiers(v.history, v.state, 2).head }
    val mod = receiveOne(5 seconds).asInstanceOf[PM]
    node ! LocallyGeneratedModifier(mod)
    expectMsgType[SyntacticallySuccessfulModifier[PM]]
    expectMsgType[SemanticallySuccessfulModifier[PM]]
  }

  property("NodeViewHolder: check state after creation") { ctx =>
    import ctx._
    node ! GetDataFromCurrentView[HT, ST, _, MPool, Boolean] { v =>
      v.state.version.sameElements(s.version)
    }
    expectMsg(true)
  }

  property("NodeViewHolder: check that a valid modifier is applicable") { ctx =>
    import ctx._
    node ! GetDataFromCurrentView[HT, ST, _, MPool, Boolean] { v =>
      v.history.applicable(mod)
    }
    expectMsg(true)
  }

  property("NodeViewHolder: check that valid modifiers are applicable") { ctx =>
    import ctx._
    node ! NodeViewHolder.Subscribe(Seq(SuccessfulSyntacticallyValidModifier, SyntacticallyFailedPersistentModifier))

    node ! GetDataFromCurrentView[HT, ST, _, MPool, Seq[PM]] { v =>
      totallyValidModifiers(v.history, v.state, 10) //todo: fix magic number
    }
    val mods = receiveOne(5 seconds).asInstanceOf[Seq[PM]]

    mods.foreach { mod =>
      node ! LocallyGeneratedModifier(mod)
    }

    (1 to mods.size).foreach(_ => expectMsgType[SyntacticallySuccessfulModifier[PM]])
  }

  property("NodeViewHolder: check sync info is synced") { ctx =>
    import ctx._
    node ! GetSyncInfo
    val syncInfo = CurrentSyncInfo(h.syncInfo(false))
    expectMsg(syncInfo)
  }

  property("NodeViewHolder: apply locally generated mod") { ctx =>
    import ctx._
    node ! NodeViewHolder.Subscribe(Seq(SuccessfulSyntacticallyValidModifier, SyntacticallyFailedPersistentModifier))

    val invalid = syntacticallyInvalidModifier(h)

    node ! LocallyGeneratedModifier(invalid)

    expectMsgType[SyntacticallyFailedModification[PM]]

    node ! LocallyGeneratedModifier(mod)

    expectMsgType[SyntacticallySuccessfulModifier[PM]]

    node ! GetDataFromCurrentView[HT, ST, _, MPool, Boolean] { v =>
      v.state.version.sameElements(s.version) && v.history.contains(mod.id)
    }

    expectMsg(true)
  }

  property("NodeViewHolder: simple forking") { ctx =>
    import ctx._

    val waitDuration = 5.seconds

    node ! NodeViewHolder.Subscribe(Seq(SuccessfulSyntacticallyValidModifier, SyntacticallyFailedPersistentModifier))

    node ! GetDataFromCurrentView[HT, ST, _, MPool, Seq[PM]] { v => totallyValidModifiers(v.history, v.state, 2) }
    val initMods = receiveOne(waitDuration).asInstanceOf[Seq[PM]]
    initMods.foreach { mod =>
      node ! LocallyGeneratedModifier(mod)
      expectMsgType[SyntacticallySuccessfulModifier[PM]]
    }

    node ! GetDataFromCurrentView[HT, ST, _, MPool, PM] { v =>
      totallyValidModifiers(v.history, v.state, 2).head
    }
    val fork1Mod = receiveOne(waitDuration).asInstanceOf[PM]

    node ! GetDataFromCurrentView[HT, ST, _, MPool, PM] { v =>
      totallyValidModifiers(v.history, v.state, 2).head
    }
    val fork2Mod = receiveOne(waitDuration).asInstanceOf[PM]

    node ! LocallyGeneratedModifier(fork1Mod)
    node ! LocallyGeneratedModifier(fork2Mod)
    expectMsgType[SyntacticallySuccessfulModifier[PM]]
    expectMsgType[SyntacticallySuccessfulModifier[PM]]

    node ! GetDataFromCurrentView[HT, ST, _, MPool, Boolean] { v =>
      v.history.contains(fork1Mod.id) || v.history.contains(fork2Mod.id)
    }


    expectMsg(10.seconds, true)
  }


  property("NodeViewHolder: forking - switching") { ctx =>
    import ctx._

    val opCountBeforeFork = 10
    val fork1OpCount = 2
    val fork2OpCount = 4

    val waitDuration = 10.seconds

    //some base operations, we don't wanna have fork right from genesis
    node ! GetDataFromCurrentView[HT, ST, _, MPool, Seq[PM]] { v =>
      totallyValidModifiers(v.history, v.state, opCountBeforeFork)
    }
    val plainMods = receiveOne(waitDuration).asInstanceOf[Seq[PM]]
    plainMods.foreach { mod => node ! LocallyGeneratedModifier(mod) }

    node ! GetDataFromCurrentView[HT, ST, _, MPool, Seq[PM]] { v =>
      val mods = totallyValidModifiers(v.history, v.state, fork1OpCount)
      assert(mods.head.parentId.sameElements(v.history.openSurfaceIds().head))
      mods
    }
    val fork1Mods = receiveOne(waitDuration).asInstanceOf[Seq[PM]]

    node ! GetDataFromCurrentView[HT, ST, _, MPool, Seq[PM]] { v =>
      totallyValidModifiers(v.history, v.state, fork2OpCount)
    }
    val fork2Mods = receiveOne(waitDuration).asInstanceOf[Seq[PM]]

    fork1Mods.foreach { mod => node ! LocallyGeneratedModifier(mod) }
    fork2Mods.foreach { mod => node ! LocallyGeneratedModifier(mod) }

    node ! GetDataFromCurrentView[HT, ST, _, MPool, Boolean] { v =>
      v.history.openSurfaceIds().contains(fork2Mods.last.id)
    }
    expectMsg(true)
  }
}
