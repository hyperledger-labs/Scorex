package scorex.core.transaction.account

import com.google.common.primitives.Longs
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

trait PublicKeyNoncedBox[PKP <: PublicKey25519Proposition, T] extends Box[PKP, T] {
  val nonce: Long

  lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val publicKey = proposition

  override def equals(obj: Any): Boolean = obj match {
    case acc: PublicKeyNoncedBox[PKP, T] => (acc.id sameElements this.id) && acc.value == this.value
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}

object PublicKeyNoncedBox {
  def idFromBox[PKP <: PublicKey25519Proposition](prop: PKP, nonce: Long): Array[Byte] =
    FastCryptographicHash(prop.pubKeyBytes ++ Longs.toByteArray(nonce))
}