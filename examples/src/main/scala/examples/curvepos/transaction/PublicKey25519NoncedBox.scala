package examples.curvepos.transaction

import com.google.common.primitives.{Ints, Longs}
import scorex.core.serialization.{BytesParseable, Serializer}
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.util.Try

case class PublicKey25519NoncedBox(
                                    override val proposition: PublicKey25519Proposition,
                                    override val nonce: Long,
                                    override val value: Long
                                  ) extends PublicKeyNoncedBox[PublicKey25519Proposition] {

  override type M = PublicKey25519NoncedBox

  override def serializer: Serializer[PublicKey25519NoncedBox] = PublicKey25519NoncedBoxSerializer
}

object PublicKey25519NoncedBoxSerializer extends Serializer[PublicKey25519NoncedBox] {


  override def bytes(obj: PublicKey25519NoncedBox): Array[Byte] =  {
    obj.proposition.pubKeyBytes ++ Longs.toByteArray(obj.nonce) ++ Longs.toByteArray(obj.value)
  } 

  override def parseBytes(bytes: Array[Byte]): Try[PublicKey25519NoncedBox] = Try {
    val pk = PublicKey25519Proposition(bytes.take(32))
    val nonce = Longs.fromByteArray(bytes.slice(32, 40))
    val value = Longs.fromByteArray(bytes.slice(40, 48))
    PublicKey25519NoncedBox(pk, nonce, value)
  }
}

