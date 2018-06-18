package scorex.core


import scorex.core.consensus.HistoryReader
import scorex.core.utils.ScorexLogging
import scorex.core.validation.RecoverableModifierError

import scala.collection.mutable
import scala.util.{Failure, Success}

/**
  * A cache which is storing persistent modifiers not applied to history yet.
  * @tparam PMOD - type of a persistent node view modifier (or a family of modifiers).
  */
trait ModifiersCache[PMOD <: PersistentNodeViewModifier, H <: HistoryReader[PMOD, _]] {
  require(maxSize >= 1)

  def size: Int = currentCacheSize

  /**
    * How many elements are to be stored in the cache
    */
  def maxSize: Int

  type K = mutable.WrappedArray[Byte]
  type V = PMOD

  protected var currentCacheSize: Int = 0

  protected val cache = mutable.Map[K, V]()

  protected val rememberedKeys = mutable.HashSet[K]()

  /**
    * Defines a best (and application-specific) candidate to be applied.
    * @param history - an interface to history which could be needed to define a candiate
    * @return - candidate if it is found
    */
  def findCandidateKey(history: H): Option[K]

  protected def onPut(key: K): Unit = {}
  protected def onRemove(key: K, rememberKey: Boolean): Unit = {}

  /**
    * A cache element replacement strategy method, which defines a key to remove from cache when it is overfull
    */
  protected def keyToRemove(): K


  def contains(key: K): Boolean = cache.contains(key) || rememberedKeys.contains(key)

  def put(key: K, value: V): Unit = synchronized {
    onPut(key)
    cache.put(key, value)
    currentCacheSize = currentCacheSize + 1

    if (currentCacheSize > maxSize) remove(keyToRemove())
  }

  /**
    * Remove an element from the cache.
    * @param key - modifier's key
    * @param rememberKey - whether to remember the key as belonging to cache. E.g. invalid modifiers are
    *                    to be remembered (for not to be requested from the network again).
    * @return
    */
  def remove(key: K, rememberKey: Boolean = false): Option[V] = synchronized {
    onRemove(key, rememberKey)
    currentCacheSize = currentCacheSize - 1
    if (rememberKey) rememberedKeys += key
    cache.remove(key)
  }

  def popCandidate(history: H): Option[V] = synchronized {
    findCandidateKey(history).flatMap(k => remove(k))
  }
}

trait LRUCache[PMOD <: PersistentNodeViewModifier, HR <: HistoryReader[PMOD, _]] extends ModifiersCache[PMOD, HR] {

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
  (override val maxSize: Int) extends ModifiersCache[PMOD, HR] with LRUCache[PMOD, HR] with ScorexLogging {

  /**
    * Default implementation is just about to scan. Not efficient at all and should be probably rewritten in a
    * concrete application.
    *
    * @param history - an interface to history which could be needed to define a candiate
    * @return - candidate if it is found
    */
  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  override def findCandidateKey(history: HR): Option[K] = {

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