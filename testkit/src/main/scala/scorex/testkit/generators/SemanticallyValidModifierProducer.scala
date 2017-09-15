package scorex.testkit.generators

import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.state.MinimalState


trait SemanticallyValidModifierProducer[PM <: PersistentNodeViewModifier, ST <: MinimalState[PM, ST]] {
  def semanticallyValidModifier(state: ST): PM
}


