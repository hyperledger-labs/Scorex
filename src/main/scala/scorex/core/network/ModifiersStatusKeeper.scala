package scorex.core.network

import java.util.concurrent.TimeUnit

import com.google.common.cache.CacheBuilder
import scorex.core.ModifierId
import scorex.core.consensus.History
import scorex.core.network.ModifiersStatusKeeper._
import scorex.crypto.encode.Base16

import scala.collection.mutable
import scalacache._
import scalacache.guava._
import scalacache.modes.sync._

/**
  *
  * Class that keeps modifiers statuses when modifier is known, but is not applied yet.
  *
  * @param maxSize     - maximum size cache to keep modifier statuseodis.
  * @param maxLivetime - maximum livetime to keep modifier statuses.
  */
class ModifiersStatusKeeper(maxSize: Long = 100000L,
                            maxLivetime: Long = 10L) {

  type K = mutable.WrappedArray[Byte]

  private val underlyingGuavaCache = CacheBuilder.newBuilder()
    .maximumSize(maxSize)
    .expireAfterWrite(maxLivetime, TimeUnit.MINUTES)
    .build[String, Entry[ModifiersStatus]]
  implicit val statuses: Cache[ModifiersStatus] = GuavaCache(underlyingGuavaCache)

  /**
    * @return status of modifier `id`.
    *         Since we do not keep statuses for already applied modifiers, `history` is required here.
    */
  def status(id: ModifierId)(history: History[_, _, _]): ModifiersStatus = {
    get(key(id)).getOrElse {
      if (history.contains(id)) {
        Applied
      } else {
        Unknown
      }
    }
  }

  def set(id: ModifierId, status: ModifiersStatus): Unit = put(key(id))(status)

  /**
    * We do not keep applied modifiers status since we can get their status from history
    */
  def applied(id: ModifierId): Unit = remove(key(id))

  /**
    * Modifier `id` was received from other peer
    */
  def received(id: ModifierId): Unit = set(id, Received)

  /**
    * Modifier `id` was requested from other peer
    */
  def requested(id: ModifierId): Unit = set(id, Requested)

  protected def key(id: ModifierId): String = Base16.encode(id)
}


object ModifiersStatusKeeper {

  sealed trait ModifiersStatus

  /**
    * This modifier is unknown to our node
    */
  case object Unknown extends ModifiersStatus

  /**
    * Our node have requested this modifier from other peers but did not received it yet.
    */
  case object Requested extends ModifiersStatus

  /**
    * Our node have received this modifier from other peers but is not applied yet.
    * It might be ModifiersCache or on the way to it
    */
  case object Received extends ModifiersStatus

  /**
    * This modifier was already applied to history
    */
  case object Applied extends ModifiersStatus

}
