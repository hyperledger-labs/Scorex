package examples.curvepos.transaction

import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.{NodeViewModifierCompanion, PersistentNodeViewModifier}

import scala.util.Try

case class SimpleNodeViewModifier(txs: Seq[FeeTransaction], generator: PublicKey25519Proposition)
  extends PersistentNodeViewModifier[PublicKey25519Proposition, FeeTransaction] {

  override type M = SimpleNodeViewModifier

  override def transactions: Option[Seq[FeeTransaction]] = Some(txs)

  override def companion: NodeViewModifierCompanion[SimpleNodeViewModifier] = SimpleNodeViewModifierCompanion

  override def id(): ModifierId =
    FastCryptographicHash(txs.map(_.messageToSign).reduce(_ ++ _) ++ generator.pubKeyBytes)

  override val modifierTypeId: ModifierTypeId = -1: Byte
}

object SimpleNodeViewModifierCompanion extends NodeViewModifierCompanion[SimpleNodeViewModifier] {
  override def bytes(modifier: SimpleNodeViewModifier): Array[ModifierTypeId] = ???

  override def parse(bytes: Array[ModifierTypeId]): Try[SimpleNodeViewModifier] = ???
}
