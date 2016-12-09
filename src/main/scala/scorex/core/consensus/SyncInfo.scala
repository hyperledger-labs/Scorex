package scorex.core.consensus

import scorex.core.serialization.BytesSerializable
import History.ModifierIds

/**
  * Syncing info provides information about starting points this node recommends another to start
  * synchronization from
  */
trait SyncInfo extends BytesSerializable {
  def answer: Boolean
  def startingPoints: ModifierIds
}


