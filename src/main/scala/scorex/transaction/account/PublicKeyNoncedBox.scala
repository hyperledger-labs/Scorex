package scorex.transaction.account

import com.google.common.primitives.Ints
import scorex.transaction.box.Box
import scorex.transaction.box.proposition.PublicKeyProposition

trait PublicKeyNoncedBox[PKP <: PublicKeyProposition] extends Box[PKP] {
  val nonce: Int

  lazy val id = proposition.id ++ Ints.toByteArray(nonce)

  lazy val publicKey = proposition.publicKey

  override def equals(obj: Any): Boolean = obj match {
    case acc: PublicKeyNoncedBox[PKP] => (acc.id sameElements this.id) && acc.value == this.value
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}
