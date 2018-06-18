package scorex.core


import scorex.core.consensus.HistoryReader
import scorex.core.utils.ScorexLogging
import scorex.core.validation.RecoverableModifierError

import scala.collection.mutable
import scala.util.{Failure, Success}


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