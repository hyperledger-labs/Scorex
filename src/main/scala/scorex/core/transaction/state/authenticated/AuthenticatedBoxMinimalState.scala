package scorex.core.transaction.state.authenticated

import scorex.crypto.authds.storage.StorageType
import scorex.crypto.authds.{AuthenticatedDictionary, DataProof}
import scorex.crypto.hash.CryptographicHash
import scorex.core.transaction.{PersistentNodeStateModifier, Transaction}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState


trait AuthenticatedBoxMinimalState[P <: Proposition, TX <: Transaction[P, TX], M <: PersistentNodeStateModifier, HashFunction <: CryptographicHash]
  extends MinimalState[P, TX, M] {

  type ElementProof <: DataProof
  type Storage <: StorageType

  protected val boxesStorage: AuthenticatedDictionary[ElementProof, Storage]

  def digest: HashFunction#Digest

  val hashFunction: HashFunction
}
