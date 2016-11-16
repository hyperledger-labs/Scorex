package scorex.core.transaction.box.proposition

import scorex.core.crypto.Secret

trait Proposition

trait ProofOfKnowledgeProposition[S <: Secret] extends Proposition

