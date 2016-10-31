package examples.hybrid.state

import com.google.common.primitives.{Ints, Longs}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import io.circe.Json
import scorex.core.NodeViewModifierCompanion
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.util.Try
import examples.hybrid.state.SimpleBoxTransaction._
import scorex.core.transaction.account.PublicKeyNoncedBox

//a transaction order to destroy boxes associated with (pubkey -> nonce) and create new boxes (pubkey -> nonce)
// where a nonce is derived from a transaction and also a box index

// WARNING!: the scheme is not provably secure to replay attacks etc
case class SimpleBoxTransaction(from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                                to: IndexedSeq[(PublicKey25519Proposition, Value)],

                                override val fee: Long,
                                override val timestamp: Long) extends
  BoxTransaction[PublicKey25519Proposition, PublicKey25519NoncedBox] {
  override type M = SimpleBoxTransaction

  lazy val boxIdsToOpen = from.map { case (prop, nonce) =>
    PublicKeyNoncedBox.idFromBox(prop, nonce)
  }

  override val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = ???
  override val newBoxes: Traversable[PublicKey25519NoncedBox] = to.zipWithIndex.map{case ((prop, value), idx) =>

    val nonce = nonceFromDigest(FastCryptographicHash(prop.pubKeyBytes ++ id ++ Ints.toByteArray(idx)))
    PublicKey25519NoncedBox(prop, nonce, value)
  }

  override lazy val companion = SimpleBoxTransactionCompanion

  override lazy val json: Json = ???


}


object SimpleBoxTransaction {
  type Value = Long
  type Nonce = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))
}

object SimpleBoxTransactionCompanion extends NodeViewModifierCompanion[SimpleBoxTransaction]{

  //todo: for Dmitry
  override def bytes(modifier: SimpleBoxTransaction): Array[Byte] = ???

  //todo: for Dmitry
  override def parse(bytes: Array[Byte]): Try[SimpleBoxTransaction] = ???
}