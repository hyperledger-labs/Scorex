package examples.curvepos.transaction

import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.{NodeViewModifierCompanion, PersistentNodeViewModifier}

case class NodeViewModifier(txs: Seq[FeeTransaction], generator: PublicKey25519Proposition)
  extends PersistentNodeViewModifier[PublicKey25519Proposition, FeeTransaction] {

  override def transactions: Option[Seq[FeeTransaction]] = Some(txs)

  override def companion: NodeViewModifierCompanion[NodeViewModifier.this.type] = ???

  override def id(): ModifierId =
    FastCryptographicHash(txs.reduce(_.messageToSign ++ _.messageToSign) ++ generator.pubKeyBytes)

  override type M = this.type
  override val modifierTypeId: ModifierTypeId = -1: Byte
}
