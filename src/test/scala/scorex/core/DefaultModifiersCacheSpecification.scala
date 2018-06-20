package scorex.core

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.consensus.{History, HistoryReader, ModifierSemanticValidity, SyncInfo}
import scorex.core.consensus.History.ModifierIds
import scorex.core.serialization.Serializer
import scorex.crypto.hash.Blake2b256

import scala.util.Try

class DefaultModifiersCacheSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  property("cache has limits") {
    class FakeModifier extends PersistentNodeViewModifier {
      override def parentId: ModifierId = ???

      override val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (0: Byte)

      override def id: ModifierId = ???

      override type M = this.type

      override def serializer: Serializer[FakeModifier.this.type] = ???
    }

    class FakeSyncInfo extends SyncInfo {
      override def startingPoints: ModifierIds = ???

      override type M = this.type

      override def serializer: Serializer[FakeSyncInfo.this.type] = ???
    }

    class FakeHr extends HistoryReader[FakeModifier, FakeSyncInfo] {
      /**
        * Is there's no history, even genesis block
        */
      override def isEmpty: Boolean = ???

      /**
        * Whether a modifier could be applied to the history
        *
        * @param modifier - modifier to apply
        * @return `Success` if modifier can be applied, `Failure(ModifierError)` if can not
        */
      override def applicableTry(modifier: FakeModifier): Try[Unit] = ???

      /**
        * Return modifier of type PM with id == modifierId
        *
        * @param modifierId - modifier id to get from history
        * @return
        */
      override def modifierById(modifierId: ModifierId): Option[FakeModifier] = ???

      /**
        * Return semantic validity status of modifier with id == modifierId
        *
        * @param modifierId - modifier id to check
        * @return
        */
      override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity = ???

      override def openSurfaceIds(): Seq[ModifierId] = ???

      /**
        * Ids of modifiers, that node with info should download and apply to synchronize
        */
      override def continuationIds(info: FakeSyncInfo, size: Int): Option[ModifierIds] = ???

      /**
        * Information about our node synchronization status. Other node should be able to compare it's view with ours by
        * this syncInfo message and calculate modifiers missed by our node.
        *
        * @return
        */
      override def syncInfo: FakeSyncInfo = ???

      /**
        * Whether another's node syncinfo shows that another node is ahead or behind ours
        *
        * @param other other's node sync info
        * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
        */
      override def compare(other: FakeSyncInfo): History.HistoryComparisonResult = ???

      override type NVCT = this.type
    }


    val v1 = Blake2b256.hash("1")
    val v2 = Blake2b256.hash("2")
    val v3 = Blake2b256.hash("3")
    val v4 = Blake2b256.hash("4")

    val cache = new DefaultModifiersCache[FakeModifier, FakeHr](3)

    cache.maxSize shouldBe 3

    cache.put(v1, new FakeModifier)
    cache.put(v2, new FakeModifier)
    cache.put(v3, new FakeModifier)

    cache.contains(v1) shouldBe true
    cache.size shouldBe 3

    cache.put(v4, new FakeModifier)
    cache.contains(v1) shouldBe false
    cache.size shouldBe 3

    cache.put(v1, new FakeModifier)
    cache.contains(v1) shouldBe true
    cache.size shouldBe 3
    cache.contains(v2) shouldBe false
  }
}
