package scorex.testkit.generators

import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.state.MinimalState


trait TotallyValidModifierProducer[PM <: PersistentNodeViewModifier, ST <: MinimalState[PM, ST],
SI <: SyncInfo, HT <: History[PM, SI, HT]] {

  def totallyValidModifier(history: HT, state: ST): PM

  def totallyValidModifiers(history: HT, state: ST, count: Int): Seq[PM]
}