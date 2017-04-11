package scorex.core.transaction.state

import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition

abstract class StateChangeOperation[P <: Proposition, BX <: Box[P]]
case class Removal[P <: Proposition, BX <: Box[P]](boxId: Array[Byte]) extends StateChangeOperation[P, BX]
case class Insertion[P <: Proposition, BX <: Box[P]](box: BX) extends StateChangeOperation[P, BX]

case class StateChanges[P <: Proposition, BX <: Box[P]] (operations: Seq[StateChangeOperation[P, BX]]){
  lazy val toAppend: Seq[Insertion[P, BX]] = operations.filter {op =>
    op match {
      case _: Insertion[P, BX] => true
      case _ => false
    }
  }.asInstanceOf[Seq[Insertion[P, BX]]]

  lazy val toRemove: Seq[Removal[P, BX]] = operations.filter {op =>
    op match {
      case _: Removal[P, BX] => true
      case _ => false
    }
  }.asInstanceOf[Seq[Removal[P, BX]]]
}
