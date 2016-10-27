package examples.hybrid.state

import examples.curvepos.transaction.PublicKey25519NoncedBox
import io.circe.Json
import scorex.core.NodeViewModifierCompanion
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.util.Try


case class SimpleBoxTransaction(override val fee: Long,
                                 override val timestamp: Long) extends
  BoxTransaction[PublicKey25519Proposition, PublicKey25519NoncedBox]{
  override type M = SimpleBoxTransaction

  override val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = ???
  override val newBoxes: Traversable[PublicKey25519NoncedBox] = ???

  override lazy val companion = SimpleBoxTransactionCompanion

  override lazy val json: Json = ???
}

object SimpleBoxTransactionCompanion extends NodeViewModifierCompanion[SimpleBoxTransaction]{
  override def bytes(modifier: SimpleBoxTransaction): Array[Byte] = ???

  override def parse(bytes: Array[Byte]): Try[SimpleBoxTransaction] = ???
}