package scorex.testkit.properties

import akka.actor._
import akka.testkit.TestProbe
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.ObjectGenerators
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier, ModifiersFromRemote}
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Vault
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.testkit.generators._
import scorex.testkit.utils.AkkaFixture
import scorex.util.ScorexLogging

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
trait NodeViewHolderTests[TX <: Transaction,
PM <: PersistentNodeViewModifier,
ST <: MinimalState[PM, ST],
SI <: SyncInfo,
HT <: History[PM, SI, HT],
MPool <: MemoryPool[TX, MPool]]
  extends AnyPropSpec
    with Matchers
    with ScorexLogging
    with SyntacticallyTargetedModifierProducer[PM, SI, HT]
    with TotallyValidModifierProducer[PM, ST, SI, HT]
    with SemanticallyInvalidModifierProducer[PM, ST]
    with CustomModifierProducer[PM, ST, SI, HT]
    with ObjectGenerators {

  def nodeViewHolder(implicit system: ActorSystem): (ActorRef, TestProbe, PM, ST, HT)

  class HolderFixture extends AkkaFixture {
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val (node, eventListener, mod, s, h) = nodeViewHolder
  }

  private def withFixture(testCode: HolderFixture => Any): Unit = {
    val fixture = new HolderFixture
    try {
      testCode(fixture)
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  private type CurrentViewType = CurrentView[HT, ST, Vault[TX, PM, _], MPool]

  private def withView[T](node: ActorRef)(f: CurrentViewType => T)
                         (implicit system: ActorSystem): T = {
    val probe = TestProbe()
    probe.send(node,
      GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, CurrentViewType] { view => view })
    val view = probe.expectMsgClass(10.seconds, classOf[CurrentViewType])
    f(view)
  }

  property("NodeViewHolder: modifiers from remote") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[ModifiersProcessingResult[PM]])
      p.send(node, ModifiersFromRemote[PM](Seq(mod)))
      eventListener.expectMsgType[ModifiersProcessingResult[PM]]
    }
  }

  property("NodeViewHolder syntactically valid modifier subscription") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier[PM]])
      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, PM] { v => totallyValidModifiers(v.history, v.state, 2).head })
      val mod = p.expectMsgClass(classOf[PersistentNodeViewModifier])
      p.send(node, LocallyGeneratedModifier(mod))
      eventListener.expectMsgType[SyntacticallySuccessfulModifier[PM]]
    }
  }

  property("NodeViewHolder: syntactically failed modifier subscription") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallyFailedModification[PM]])
      val invalid = syntacticallyInvalidModifier(h)
      p.send(node, LocallyGeneratedModifier(invalid))
      eventListener.expectMsgType[SyntacticallyFailedModification[PM]]
    }
  }

  property("NodeViewHolder: semantically valid modifier subscription") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier[PM]])
      system.eventStream.subscribe(eventListener.ref, classOf[SemanticallySuccessfulModifier[PM]])
      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, PM] { v => totallyValidModifiers(v.history, v.state, 2).head })
      val mod = p.expectMsgClass(classOf[PersistentNodeViewModifier])
      p.send(node, LocallyGeneratedModifier(mod))
      eventListener.expectMsgType[SyntacticallySuccessfulModifier[PM]]
      eventListener.expectMsgType[SemanticallySuccessfulModifier[PM]]
    }
  }

  property("NodeViewHolder: semantically failed modifier subscription") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier[PM]])
      system.eventStream.subscribe(eventListener.ref, classOf[SemanticallyFailedModification[PM]])
      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, PM] { v => semanticallyInvalidModifier(v.state) })
      val invalid = p.expectMsgClass(classOf[PersistentNodeViewModifier])
      p.send(node, LocallyGeneratedModifier(invalid))
      eventListener.expectMsgType[SyntacticallySuccessfulModifier[PM]]
      eventListener.expectMsgType[SemanticallyFailedModification[PM]]
    }
  }

  property("NodeViewHolder: syntactically/semantically valid modifier subscription") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier[PM]])
      system.eventStream.subscribe(eventListener.ref, classOf[SemanticallySuccessfulModifier[PM]])
      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, PM] { v => totallyValidModifiers(v.history, v.state, 2).head })
      val mod = p.expectMsgClass(classOf[PersistentNodeViewModifier])
      p.send(node, LocallyGeneratedModifier(mod))
      eventListener.expectMsgType[SyntacticallySuccessfulModifier[PM]]
      eventListener.expectMsgType[SemanticallySuccessfulModifier[PM]]
    }
  }

  property("NodeViewHolder: check state after creation") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, Boolean] { v =>
        v.state.version == s.version
      })
      p.expectMsg(true)
    }
  }

  property("NodeViewHolder: check that a valid modifier is applicable") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, Boolean] { v =>
        v.history.applicableTry(mod).isSuccess
      })
      p.expectMsg(true)
    }
  }

  property("NodeViewHolder: check that valid modifiers are applicable") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier[PM]])
      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallyFailedModification[PM]])
      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, Seq[PM]] { v =>
        totallyValidModifiers(v.history, v.state, 10) //todo: fix magic number
      })
      val mods = p.expectMsgClass(classOf[Seq[PersistentNodeViewModifier]])

      mods.foreach { mod =>
        p.send(node, LocallyGeneratedModifier(mod))
      }

      (1 to mods.size).foreach(_ => eventListener.expectMsgType[SyntacticallySuccessfulModifier[PM]])
    }
  }

  property("NodeViewHolder: apply locally generated mod") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier[PM]])
      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallyFailedModification[PM]])

      val invalid = syntacticallyInvalidModifier(h)

      p.send(node, LocallyGeneratedModifier(invalid))

      eventListener.expectMsgType[SyntacticallyFailedModification[PM]]

      p.send(node, LocallyGeneratedModifier(mod))

      eventListener.expectMsgType[SyntacticallySuccessfulModifier[PM]]

      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, Boolean] { v =>
        v.state.version == s.version && v.history.contains(mod.id)
      })

      p.expectMsg(true)
    }
  }

  property("NodeViewHolder: simple forking") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      val waitDuration = 5.seconds

      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallySuccessfulModifier[PM]])
      system.eventStream.subscribe(eventListener.ref, classOf[SyntacticallyFailedModification[PM]])

      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, Seq[PM]] { v => totallyValidModifiers(v.history, v.state, 2) })
      val initMods = p.expectMsgClass(waitDuration, classOf[Seq[PersistentNodeViewModifier]])
      initMods.foreach { mod =>
        p.send(node, LocallyGeneratedModifier(mod))
        eventListener.expectMsgType[SyntacticallySuccessfulModifier[PM]]
      }

      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, PM] { v =>
        totallyValidModifiers(v.history, v.state, 2).head
      })
      val fork1Mod = p.expectMsgClass(waitDuration, classOf[PersistentNodeViewModifier])

      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, PM] { v =>
        totallyValidModifiers(v.history, v.state, 2).head
      })
      val fork2Mod = p.expectMsgClass(waitDuration, classOf[PersistentNodeViewModifier])

      p.send(node, LocallyGeneratedModifier(fork1Mod))
      p.send(node, LocallyGeneratedModifier(fork2Mod))
      eventListener.expectMsgType[SyntacticallySuccessfulModifier[PM]]
      eventListener.expectMsgType[SyntacticallySuccessfulModifier[PM]]

      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, Boolean] { v =>
        v.history.contains(fork1Mod.id) || v.history.contains(fork2Mod.id)
      })

      p.expectMsg(10.seconds, true)
    }
  }

  /**
    * In this test we apply first a chain of 2 blocks and then a chain of 4 blocks, both started with the same
    * common block. We are expecting to observe "switching" here, though with non-chain structures there could be no
    * notion of switching, so what we check finally is that last block from the second chain is in "open surface"
    * (list of open blocks which do not have successors yet, size of the list is 1 in case of blockchain)
    */
  property("NodeViewHolder: forking - switching") {
    withFixture { ctx =>
      import ctx._
      val p = TestProbe()

      val opCountBeforeFork = 10
      val fork1OpCount = 2
      val fork2OpCount = 4

      val waitDuration = 10.seconds

      //some base operations, we don't wanna have fork right from genesis
      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, Seq[PM]] { v =>
        totallyValidModifiers(v.history, v.state, opCountBeforeFork)
      })
      val plainMods = p.expectMsgClass(waitDuration, classOf[Seq[PersistentNodeViewModifier]])
      plainMods.foreach { mod => p.send(node, LocallyGeneratedModifier(mod)) }

      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, Seq[PM]] { v =>
        val mods = totallyValidModifiers(v.history, v.state, fork1OpCount)
        assert(mods.head.parentId == v.history.openSurfaceIds().head)
        mods
      })
      val fork1Mods = p.expectMsgClass(waitDuration, classOf[Seq[PersistentNodeViewModifier]])

      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, Seq[PM]] { v =>
        totallyValidModifiers(v.history, v.state, fork2OpCount)
      })
      val fork2Mods = p.expectMsgClass(waitDuration, classOf[Seq[PersistentNodeViewModifier]])

      fork1Mods.foreach { mod => p.send(node, LocallyGeneratedModifier(mod)) }
      fork2Mods.foreach { mod => p.send(node, LocallyGeneratedModifier(mod)) }

      p.send(node, GetDataFromCurrentView[HT, ST, Vault[TX, PM, _], MPool, Boolean] { v =>
        v.history.openSurfaceIds().contains(fork2Mods.last.id)
      })
      p.expectMsg(true)
    }
  }

  property("NodeViewHolder: forking - switching with an invalid block") {
    withFixture { ctx =>
      import ctx._

      val opCountBeforeFork = 10
      val fork1OpCount = 4

      //some base operations, we don't wanna have fork right from genesis
      withView(node) { v =>
        totallyValidModifiers(v.history, v.state, opCountBeforeFork)
      }.foreach {
        mod => node ! LocallyGeneratedModifier(mod)
      }
      // generate the first fork with valid blocks
      val fork1Mods = withView(node) { v =>
        val mods = totallyValidModifiers(v.history, v.state, fork1OpCount)
        assert(mods.head.parentId == v.history.openSurfaceIds().head)
        mods
      }
      // generate the second fork with the invalid block
      val fork2Mods = withView(node) { v =>
        customModifiers(v.history, v.state,
          Seq[ModifierProducerTemplateItem](Valid,
            SynInvalid, // invalid modifier
            Valid, Valid, Valid, Valid, Valid, Valid))
      }
      // apply the first fork with valid blocks
      fork1Mods.foreach { mod => node ! LocallyGeneratedModifier(mod) }
      // apply the second fork with invalid block
      fork2Mods.foreach { mod => node ! LocallyGeneratedModifier(mod) }
      // verify that open surface consist of last block of the first chain,
      // or first block of the second chain, or both, but no any other option
      withView(node) { v =>
        v.history.openSurfaceIds should (
          contain only fork1Mods.last.id
            or contain only fork2Mods.head.id
            or contain only(fork1Mods.last.id, fork2Mods.head.id)
          )
      }
    }
  }

}