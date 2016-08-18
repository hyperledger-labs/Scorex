package scorex.transaction.box.proposition

import scorex.serialization.BytesSerializable
import scorex.transaction.state.Secret

trait Proposition extends BytesSerializable

trait ProofOfKnowledgeProposition[S <: Secret] extends Proposition

