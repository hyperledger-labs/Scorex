package scorex.core

import scorex.core.consensus.HistoryReader
import scorex.core.utils.ScorexLogging
import scorex.core.validation.RecoverableModifierError

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success}

/**
  * A cache which is storing persistent modifiers not applied to history yet.
  *
  * @tparam PMOD - type of a persistent node view modifier (or a family of modifiers).
  */
trait ModifiersCache[PMOD <: PersistentNodeViewModifier, H <: HistoryReader[PMOD, _]] {
  require(maxSize >= 1)

  type K = ModifierId
  type V = PMOD

  protected val cache: mutable.Map[K, V] = mutable.Map[K, V]()

  def size: Int = cache.size

  /**
    * How many elements are to be stored in the cache
    */
  def maxSize: Int

  /**
    * Defines a best (and application-specific) candidate to be applied.
    *
    * @param history - an interface to history which could be needed to define a candidate
    * @return - candidate if it is found
    */
  def findCandidateKey(history: H): Option[K]

  protected def onPut(key: K): Unit = {}

  protected def onRemove(key: K): Unit = {}

  /**
    * Remove elements from cache when it is overfull
    *
    * @return collection of just removed elements
    */
  def cleanOverfull(): Seq[V]

  def contains(key: K): Boolean = cache.contains(key)

  def put(key: K, value: V): Unit = synchronized {
    if (!contains(key)) {
      onPut(key)
      cache.put(key, value)
    }
  }

  /**
    * Remove an element from the cache.
    *
    * @param key - modifier's key
    * @return - removed value if existed
    */
  def remove(key: K): Option[V] = synchronized {
    cache.remove(key).map { removed =>
      onRemove(key)
      removed
    }
  }

  def popCandidate(history: H): Option[V] = synchronized {
    findCandidateKey(history).flatMap(k => remove(k))
  }
}

trait LRUCache[PMOD <: PersistentNodeViewModifier, HR <: HistoryReader[PMOD, _]] extends ModifiersCache[PMOD, HR] {

  private val evictionQueue = mutable.Queue[K]()

  // The eviction queue can contain elements already removed, as we're not removing a key from it when
  // the key is got removed from the cache. When size of eviction queue exceeds maximum size of the cache by
  // the value below(so the queue contains at least "cleaningThreshold" keys aleady removed from the cache),
  // complete scan and cleaning of removed keys happen.
  private val cleaningThreshold = 50


  override protected def onPut(key: K): Unit = {
    evictionQueue.enqueue(key)
    if (evictionQueue.lengthCompare(maxSize + cleaningThreshold) > 0) {
      evictionQueue.dequeueAll(k => !cache.contains(k))
    }
  }

  def cleanOverfull(): Seq[V] = {
    @tailrec
    def removeUntilCorrectSize(acc: List[V]): List[V] = if (size <= maxSize || evictionQueue.isEmpty) {
      acc
    } else {
      removeUntilCorrectSize(remove(evictionQueue.dequeue()).map(_ :: acc).getOrElse(acc))
    }

    removeUntilCorrectSize(List())
  }
}

class DefaultModifiersCache[PMOD <: PersistentNodeViewModifier, HR <: HistoryReader[PMOD, _]]
(override val maxSize: Int) extends ModifiersCache[PMOD, HR] with LRUCache[PMOD, HR] with ScorexLogging {

  /**
    * Default implementation is just about to scan. Not efficient at all and should be probably rewritten in a
    * concrete application.
    *
    * @param history - an interface to history which could be needed to define a candidate
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
          log.warn(s"Modifier ${v.encodedId} became permanently invalid and will be removed from cache", e)
          remove(k)
          false
        case Success(_) =>
          true
      }
    }.map(_._1)
  }
}