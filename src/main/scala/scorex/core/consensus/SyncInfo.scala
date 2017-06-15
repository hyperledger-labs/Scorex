package scorex.core.consensus

import scorex.core.serialization.BytesSerializable

/**
  * Syncing info provides information about starting points this node recommends another to start
  * synchronization from
  */
trait SyncInfo extends BytesSerializable {
  /**
    * Is this the answer for request
    */
  def answer: Boolean
  def startingPoints: History.ModifierIds
}


