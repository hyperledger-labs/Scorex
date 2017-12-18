package scorex.core.consensus

object ModifierSemanticValidity extends Enumeration {
  //TODO add comments
  val Absent = Value(0)
  val Unknown = Value(1)
  val Valid = Value(2)
  val Invalid = Value(3)
}