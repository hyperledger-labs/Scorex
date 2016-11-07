package examples.curvepos.transaction

import com.google.common.primitives.{Ints, Longs}
import scorex.core.serialization.BytesParseable
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.PublicKey25519Proposition


import scala.util.Try

case class PublicKey25519NoncedBox(
                                    override val proposition: PublicKey25519Proposition,
                                    override val nonce: Long,
                                    override val value: Long
                                  ) extends PublicKeyNoncedBox[PublicKey25519Proposition] {
  override lazy val bytes: Array[Byte] =
    proposition.pubKeyBytes ++ Longs.toByteArray(nonce) ++ Longs.toByteArray(value)
}

object PublicKey25519NoncedBox extends BytesParseable[PublicKey25519NoncedBox] {
  override def parseBytes(bytes: Array[Byte]): Try[PublicKey25519NoncedBox] = Try {
    val pk = PublicKey25519Proposition(bytes.take(32))
    val nonce = Ints.fromByteArray(bytes.slice(32, 40))
    val value = Longs.fromByteArray(bytes.slice(40, 48))
    PublicKey25519NoncedBox(pk, nonce, value)
  }
}

