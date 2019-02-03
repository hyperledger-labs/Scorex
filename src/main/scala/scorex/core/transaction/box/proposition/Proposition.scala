package scorex.core.transaction.box.proposition

import scorex.core.serialization.BytesSerializable
import scorex.core.transaction.state.Secret

trait Proposition extends BytesSerializable

trait ProofOfKnowledgeProposition[S <: Secret] extends Proposition

