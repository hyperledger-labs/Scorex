package scorex.core.transaction

import com.google.common.primitives.Longs
import io.circe.Json
import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.box.{Box, BoxUnlocker}
import scala.util.Try


trait NodeViewModifier {
  self =>

  import NodeViewModifier.{ModifierId, ModifierTypeId}

  type M >: self.type <: NodeViewModifier

  val modifierTypeId: ModifierTypeId

  //todo: check statically or dynamically output size
  def id: ModifierId

  def companion: NodeViewModifierCompanion[M]
}

trait NodeViewModifierCompanion[M <: NodeViewModifier] {
  def bytes(modifier: M): Array[Byte]

  def parse(bytes: Array[Byte]): Try[M]

  def bytes(modifiers: Seq[M]): Seq[Array[Byte]] = modifiers.map(bytes)
}

/**
  * It is supposed that all the modifiers (offchain transactions, blocks, blockheaders etc)
  * are of the some length fixed with the ModifierIdSize constant
  */
object NodeViewModifier {
  type ModifierTypeId = Byte
  type ModifierId = Array[Byte]

  val ModifierIdSize: Int = 32 //todo: make configurable via application.conf
}

trait PersistentNodeViewModifier[P <: Proposition, TX <: Transaction[P]] extends NodeViewModifier {

  // with Dotty is would be Seq[TX] | Nothing
  def transactions: Option[Seq[TX]]
}

case class TransactionChanges[P <: Proposition](toRemove: Set[Box[P]], toAppend: Set[Box[P]], minerReward: Long)


/**
  * A transaction is an atomic state modifier
  */

abstract class Transaction[P <: Proposition]
  extends NodeViewModifier with JsonSerializable {

  override val modifierTypeId: Byte = Transaction.TransactionModifierId

  val fee: Long

  val timestamp: Long

  def json: Json

  val messageToSign: Array[Byte]
}

object Transaction {
  val TransactionModifierId = 2: Byte
}

abstract class BoxTransaction[P <: Proposition] extends Transaction[P] {

  val unlockers: Traversable[BoxUnlocker[P]]
  val newBoxes: Traversable[Box[P]]

  override lazy val messageToSign: Array[Byte] =
    newBoxes.map(_.bytes).reduce(_ ++ _) ++
      unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
}