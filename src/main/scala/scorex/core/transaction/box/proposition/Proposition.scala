package scorex.core.transaction.box.proposition

import scorex.core.transaction.state.Secret

trait Proposition

trait ProofOfKnowledgeProposition[S <: Secret] extends Proposition

