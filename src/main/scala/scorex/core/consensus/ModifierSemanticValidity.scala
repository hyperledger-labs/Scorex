package scorex.core.consensus

sealed trait ModifierSemanticValidity {
  val code: Byte
}

object ModifierSemanticValidity {
  def restoreFromCode(code: Byte): ModifierSemanticValidity = code match {
    case b: Byte if b == Absent.code => Absent
    case b: Byte if b == Unknown.code => Unknown
    case b: Byte if b == Valid.code => Valid
    case b: Byte if b == Invalid.code => Invalid
  }
}

case object Absent extends ModifierSemanticValidity {
  override val code = 0
}

case object Unknown extends ModifierSemanticValidity {
  override val code = 1
}

case object Valid extends ModifierSemanticValidity {
  override val code = 2
}

case object Invalid extends ModifierSemanticValidity {
  override val code = 3
}