package examples.curvepos.transaction

import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.{NodeViewModifierCompanion, PersistentNodeViewModifier}

import scala.util.Try

case class SimpleBlock(txs: Seq[SimpleTransaction], generator: PublicKey25519Proposition)
  extends PersistentNodeViewModifier[PublicKey25519Proposition, SimpleTransaction] {

  override type M = SimpleBlock

  override def transactions: Option[Seq[SimpleTransaction]] = Some(txs)

  override def companion: NodeViewModifierCompanion[SimpleBlock] = SimpleNodeViewModifierCompanion

  override def id: ModifierId =
    FastCryptographicHash(txs.map(_.messageToSign).reduce(_ ++ _) ++ generator.pubKeyBytes)

  override val modifierTypeId: ModifierTypeId = -1: Byte
}

object SimpleNodeViewModifierCompanion extends NodeViewModifierCompanion[SimpleBlock] {
  override def bytes(modifier: SimpleBlock): Array[ModifierTypeId] = ???

  override def parse(bytes: Array[ModifierTypeId]): Try[SimpleBlock] = ???
}
