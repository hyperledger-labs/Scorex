package scorex.core

import com.typesafe.config.ConfigFactory
import scorex.core.serialization.{BytesSerializable, JsonSerializable}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.encode.Base58

import scala.util.Try

sealed trait NodeViewModifier extends BytesSerializable with JsonSerializable {
  self =>

  val modifierTypeId: ModifierTypeId

  //todo: check statically or dynamically output size
  def id: ModifierId

  def encodedId: String = Base58.encode(id)

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: NodeViewModifier => (that.id sameElements id) && (that.modifierTypeId == modifierTypeId)
    case _ => false
  }
}

trait EphemerealNodeViewModifier extends NodeViewModifier

/**
  * It is supposed that all the modifiers (offchain transactions, blocks, blockheaders etc)
  * have identifiers of the some length fixed with the ModifierIdSize constant
  */
object NodeViewModifier {
  val DefaultIdSize = 32 // in bytes

  val ModifierIdSize: Int = Try(ConfigFactory.load().getConfig("app").getInt("modifierIdSize")).getOrElse(DefaultIdSize)
}


trait PersistentNodeViewModifier extends NodeViewModifier {
  def parentId: ModifierId
}


trait TransactionsCarryingPersistentNodeViewModifier[P <: Proposition, TX <: Transaction[P]]
  extends PersistentNodeViewModifier {

  def transactions: Seq[TX]
}
