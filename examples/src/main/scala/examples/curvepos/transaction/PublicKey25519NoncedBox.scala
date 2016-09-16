package examples.curvepos.transaction

import com.google.common.primitives.{Ints, Longs}
import scorex.core.serialization.BytesParseable
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PublicKey25519

import scala.util.Try

case class PublicKey25519NoncedBox(
                                    override val proposition: PublicKey25519Proposition,
                                    override val nonce: Int,
                                    override val value: Long
                                  ) extends PublicKeyNoncedBox[PublicKey25519Proposition] {
  override lazy val bytes: Array[Byte] =
    proposition.publicKey.publicKeyBytes ++ Ints.toByteArray(nonce) ++ Longs.toByteArray(value)
}

object PublicKey25519NoncedBox extends BytesParseable[PublicKey25519NoncedBox] {
  def apply(proposition: PublicKey25519Proposition, value: Long): PublicKey25519NoncedBox =
    PublicKey25519NoncedBox(proposition, 0, value)

  override def parseBytes(bytes: Array[Byte]): Try[PublicKey25519NoncedBox] = Try {
    val pk = PublicKey25519Proposition(PublicKey25519(bytes.take(32)))
    val nonce = Ints.fromByteArray(bytes.slice(32, 36))
    val value = Longs.fromByteArray(bytes.slice(36, 44))
    PublicKey25519NoncedBox(pk, nonce, value)
  }
}

