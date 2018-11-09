package scorex.core.consensus

/**
  * Syncing info provides information about starting points this node recommends another to start
  * synchronization from
  */
trait SyncInfo {
  def startingPoints: History.ModifierIds
}


