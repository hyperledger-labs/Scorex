package scorex.testkit.generators

import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}


trait SyntaticallyValidModifierProducer[PM <: PersistentNodeViewModifier, SI <: SyncInfo, HT <: History[PM, SI, HT]] {
  def syntaticallyValidModifier(history: HT): PM
}
