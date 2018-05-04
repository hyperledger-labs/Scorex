package scorex.testkit.generators

import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.state.MinimalState

sealed trait ModifierProducerTemplateItem

case object SynInvalid extends ModifierProducerTemplateItem
case object Valid extends ModifierProducerTemplateItem

trait CustomModifierProducer[PM <: PersistentNodeViewModifier, ST <: MinimalState[PM, ST],
SI <: SyncInfo, HT <: History[PM, SI, HT]] {

  def customModifiers(history: HT,
                      state: ST,
                      template: Seq[ModifierProducerTemplateItem]): Seq[PM]
}
