package scorex.testkit.generators

import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}


trait SyntacticallyTargetedModifierProducer[PM <: PersistentNodeViewModifier, SI <: SyncInfo, HT <: History[PM, SI, HT]] {
  def syntacticallyValidModifier(history: HT): PM

  def syntacticallyInvalidModifier(history: HT): PM
}
