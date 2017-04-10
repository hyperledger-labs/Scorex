package examples.tailchain.modifiers

import examples.commons.SimpleBoxTransaction
import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import examples.tailchain.core.Constants._
import examples.tailchain.core.Ticket


case class BlockHeader(override val parentId: ModifierId,
                       stateRoot: StateRoot,
                       txRoot: TransactionsRoot,
                       ticket: Ticket,
                       powNonce: Long
                      ) extends TModifier {
  // with Dotty is would be Seq[TX] | Nothing
  override def transactions: Option[Seq[SimpleBoxTransaction]] = None

  override val modifierTypeId: ModifierTypeId = TModifier.Header

  //todo: for Dmitry: implement as hash of the header
  override lazy val id: ModifierId = ???

  //todo: for Dmitry: implement
  override def json: Json = ???

  override type M = this.type

  //todo: for Dmitry: implement
  override def serializer: Serializer[BlockHeader.this.type] = ???
}
