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
      override def isEmpty: Boolean = ???
      override def applicableTry(modifier: FakeModifier): Try[Unit] = ???
      override def modifierById(modifierId: ModifierId): Option[FakeModifier] = ???
      override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity = ???
      override def openSurfaceIds(): Seq[ModifierId] = ???
      override def continuationIds(info: FakeSyncInfo, size: Int): Option[ModifierIds] = ???
      override def syncInfo: FakeSyncInfo = ???
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