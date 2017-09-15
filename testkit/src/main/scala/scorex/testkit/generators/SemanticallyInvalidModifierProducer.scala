package scorex.testkit.generators

import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.state.MinimalState


trait SemanticallyInvalidModifierProducer[PM <: PersistentNodeViewModifier, ST <: MinimalState[PM, ST]] {
  def semanticallyInvalidModifier(state: ST): PM
}
