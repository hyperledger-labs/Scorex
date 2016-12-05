package scorex.core

import com.typesafe.config.ConfigFactory
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.serialization.{BytesSerializable, JsonSerializable, Serializer}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition

import scala.util.Try

trait NodeViewModifier extends BytesSerializable with JsonSerializable {
  self =>

  import NodeViewModifier.{ModifierId, ModifierTypeId}

  val modifierTypeId: ModifierTypeId

  //todo: check statically or dynamically output size
  def id: ModifierId
}

/**
  * It is supposed that all the modifiers (offchain transactions, blocks, blockheaders etc)
  * have identifiers of the some length fixed with the ModifierIdSize constant
  */
object NodeViewModifier {
  val DefaultIdSize = 32 // in bytes

  type ModifierTypeId = Byte
  type ModifierId = Array[Byte]

  val ModifierIdSize: Int = Try(ConfigFactory.load().getConfig("app").getInt("modifierIdSize")).getOrElse(DefaultIdSize)
}



trait PersistentNodeViewModifier[P <: Proposition, TX <: Transaction[P]] extends NodeViewModifier {

  def parentId: ModifierId

  // with Dotty is would be Seq[TX] | Nothing
  def transactions: Option[Seq[TX]]
}