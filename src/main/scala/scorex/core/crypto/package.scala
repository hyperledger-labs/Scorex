package scorex.core

// The purpose of this package is to act as a single interface
// between scorex.core and scorex.crypto.
//
// This is the only file in scorex.core that directly depends
// on scorex.crypto

package object crypto {
  import scorex.crypto.{ hash => h }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object hash {
    val Blake2b256 = h.Blake2b256
    type Blake2b256 = h.Blake2b256.type
    type Digest = h.Digest

    val Sha256 = h.Sha256 // only used in Examples
    type CryptographicHash[D <: Digest] = h.CryptographicHash[D] // only used in Examples
    type Digest32 = h.Digest32 // only used in Examples
  }
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object signatures {
    import scorex.crypto.{ signatures => s }
    val Curve25519 = s.Curve25519
    val PublicKey = s.PublicKey
    type PublicKey = s.PublicKey
    val PrivateKey = s.PrivateKey
    type PrivateKey = s.PrivateKey
    val Signature = s.Signature
    type Signature = s.Signature
  }
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object encode {
    import scorex.crypto.{ encode => e }
    val Base16 = e.Base16
    val Base58 = e.Base58
    type BytesEncoder = e.BytesEncoder
  }
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object authds {
    import scorex.crypto.{ authds => a }
    val ADKey = a.ADKey
    type ADKey = a.ADKey

    val ADValue = a.ADValue // Only used in Examples
    type ADValue = a.ADValue // Only used in Examples
    val SerializedAdProof = a.SerializedAdProof // Only used in Examples
    type SerializedAdProof = a.SerializedAdProof // Only used in Examples
    val ADDigest = a.ADDigest // Only used in Examples
    type ADDigest = a.ADDigest // Only used in Examples

    object avltree { // Only used in Examples
      object batch {
        import a.avltree.{ batch => b }
        type BatchAVLVerifier[D <: h.Digest, HF <: h.CryptographicHash[D]] = b.BatchAVLVerifier[D, HF]
        type BatchAVLProver[D <: h.Digest, HF <: h.CryptographicHash[D]] = b.BatchAVLProver[D, HF]
        val Insert = b.Insert
        val Lookup = b.Lookup
        val Remove = b.Remove
      }
    }
  }
}
