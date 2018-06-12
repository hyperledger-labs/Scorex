package scorex.core

import com.typesafe.config.ConfigFactory
import scorex.core.serialization.BytesSerializable
import scorex.core.transaction.Transaction
import scorex.core.utils.ScorexLogging

import scala.util.Try

sealed trait NodeViewModifier extends BytesSerializable with ScorexLogging {
  self =>

  val modifierTypeId: ModifierTypeId

  //todo: check statically or dynamically output size
  def id: ModifierId

  def encodedId: String = encoder.encode(id)

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
  private val DefaultIdSize = 32 // in bytes, TODO: should we use type Byte?

  val ModifierIdSize: Int = Try(ConfigFactory.load().getConfig("app").getInt("modifierIdSize")).getOrElse(DefaultIdSize)
}


trait PersistentNodeViewModifier extends NodeViewModifier {
  def parentId: ModifierId
}


trait TransactionsCarryingPersistentNodeViewModifier[TX <: Transaction]
  extends PersistentNodeViewModifier {

  def transactions: Seq[TX]
}
