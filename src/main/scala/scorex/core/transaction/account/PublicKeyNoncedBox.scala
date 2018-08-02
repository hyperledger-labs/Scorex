package scorex.core.transaction.account

import com.google.common.primitives.Longs
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256

trait PublicKeyNoncedBox[PKP <: PublicKey25519Proposition] extends Box[PKP] {
  val nonce: Long

  lazy val id: ADKey = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val publicKey: PKP = proposition

  override def equals(obj: Any): Boolean = obj match {
    case acc: PublicKeyNoncedBox[PKP] => java.util.Arrays.equals(acc.id, this.id) && acc.value == this.value
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}

object PublicKeyNoncedBox {
  def idFromBox[PKP <: PublicKey25519Proposition](prop: PKP, nonce: Long): ADKey =
    ADKey @@ Blake2b256(prop.pubKeyBytes ++ Longs.toByteArray(nonce))
}