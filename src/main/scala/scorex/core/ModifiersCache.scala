package scorex.core


import scorex.core.consensus.History.ModifierIds
import scorex.core.consensus.{History, HistoryReader, ModifierSemanticValidity, SyncInfo}
import scorex.core.serialization.Serializer
import scorex.core.utils.ScorexLogging
import scorex.core.validation.RecoverableModifierError
import scorex.crypto.hash.Blake2b256

import scala.collection.mutable
import scala.util.{Failure, Success, Try}


trait ModifiersCache[PMOD <: PersistentNodeViewModifier] {
  require(maxSize >= 1)

  type H <: HistoryReader[PMOD, _]

  type K = mutable.WrappedArray[Byte]
  type V = PMOD

  protected var currentCacheSize: Int = 0

  protected val cache = mutable.Map[K, V]()

  protected val rememberedKeys = mutable.HashSet[K]()

  def findCandidateKey(history: H): Option[K]

  protected def onPut(key: K): Unit = {}
  protected def onRemove(key: K, rememberKey: Boolean): Unit = {}
  protected def keyToRemove(): K


  def contains(key: K): Boolean = cache.contains(key) || rememberedKeys.contains(key)

  def put(key: K, value: V): Unit = synchronized {
    onPut(key)
    cache.put(key, value)
    currentCacheSize = currentCacheSize + 1

    if (currentCacheSize > maxSize) remove(keyToRemove())
  }

  // def get(key: K): Option[V] = cache.get(key)

  def remove(key: K, rememberKey: Boolean = false): Option[V] = synchronized {
    onRemove(key, rememberKey)
    currentCacheSize = currentCacheSize - 1
    if (rememberKey) rememberedKeys += key
    cache.remove(key)
  }

  def popCandidate(history: H): Option[V] = synchronized {
    findCandidateKey(history).flatMap(k => remove(k))
  }

  def size: Int = currentCacheSize

  def maxSize: Int
}

trait LRUCache[PMOD <: PersistentNodeViewModifier] extends ModifiersCache[PMOD] {

  type Counter = Long

  private var cnt = 0: Counter

  private val keyIndex = mutable.Map[K, Counter]()

  override protected def onPut(key: K): Unit = {
    keyIndex.put(key, cnt)
    cnt = cnt + 1
  }

  override protected def onRemove(key: K, rememberKey: Boolean): Unit = {
    keyIndex.remove(key)
  }

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def keyToRemove(): K = {
    val (key, _) = keyIndex.minBy(_._2)  //todo: can we do faster search?
    keyIndex.remove(key)
    key
  }
}

class DefaultModifiersCache[PMOD <: PersistentNodeViewModifier, HR <: HistoryReader[PMOD, _]]
  (override val maxSize: Int) extends ModifiersCache[PMOD] with LRUCache[PMOD] with ScorexLogging {

  override type H = HR

  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  override def findCandidateKey(history: H): Option[K] = {

    cache.find { case (k, v) =>
      history.applicableTry(v) match {
        case Failure(e) if e.isInstanceOf[RecoverableModifierError] =>
          // do nothing - modifier may be applied in future
          false
        case Failure(e) =>
          // non-recoverable error - remove modifier from cache
          // TODO blaklist peer who sent it
          log.warn(s"Modifier ${v.encodedId} is permanently invalid and will be removed from cache", e)
          remove(k, rememberKey = true)
          false
        case Success(_) =>
          true
      }
    }.map(_._1)
  }
}

object DefaultModifiersCache extends App {

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


  private val v1 = Blake2b256.hash("1")
  private val v2 = Blake2b256.hash("2")
  private val v3 = Blake2b256.hash("3")
  private val v4 = Blake2b256.hash("4")

  private val cache = new DefaultModifiersCache[FakeModifier, FakeHr](3)

  cache.put(v1, new FakeModifier)
  cache.put(v2, new FakeModifier)
  cache.put(v3, new FakeModifier)

  println(cache.contains(v1))

  cache.put(v4, new FakeModifier)

  println(cache.contains(v1))

  cache.put(v1, new FakeModifier)
  println(cache.contains(v1))
}