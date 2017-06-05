package scorex.core.transaction.state

import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition

abstract class StateChangeOperation[T, P <: Proposition, BX <: Box[P, T]]
case class Removal[T, P <: Proposition, BX <: Box[P, T]](boxId: Array[Byte]) extends StateChangeOperation[T, P, BX]
case class Insertion[T, P <: Proposition, BX <: Box[P, T]](box: BX) extends StateChangeOperation[T, P, BX]

case class StateChanges[T, P <: Proposition, BX <: Box[P, T]] (operations: Seq[StateChangeOperation[T, P, BX]]){
  lazy val toAppend: Seq[Insertion[T, P, BX]] = operations.filter {op =>
    op match {
      case _: Insertion[T, P, BX] => true
      case _ => false
    }
  }.asInstanceOf[Seq[Insertion[T, P, BX]]]

  lazy val toRemove: Seq[Removal[T, P, BX]] = operations.filter {op =>
    op match {
      case _: Removal[T, P, BX] => true
      case _ => false
    }
  }.asInstanceOf[Seq[Removal[T, P, BX]]]
}
