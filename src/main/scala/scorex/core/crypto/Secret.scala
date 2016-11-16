package scorex.core.crypto

import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition
import scorex.core.transaction.proof.ProofOfKnowledge

trait Secret {
  self =>
  type S >: self.type <: Secret
  type PK <: ProofOfKnowledgeProposition[S]

  def companion: SecretCompanion[S]

  def instance: S = self

  def publicImage: PK
}

trait SecretCompanion[S <: Secret] {
  type PK = S#PK

  type PR <: ProofOfKnowledge[S, _ <: ProofOfKnowledgeProposition[S]]

  def owns(secret: S, box: Box[_]): Boolean

  def sign(secret: S, message: Array[Byte]): PR

  //TODO not used
  def verify(message: Array[Byte], publicImage: PK, proof: PR): Boolean

  def generateKeys(randomSeed: Array[Byte]): (S, PK)
}
