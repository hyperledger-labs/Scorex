package scorex.core.consensus

sealed trait ModifierSemanticValidity {
  val code: Byte
}

object ModifierSemanticValidity {
  def restoreFromCode(code: Byte): ModifierSemanticValidity =
    if (code == Valid.code) Valid
    else if (code == Unknown.code) Unknown
    else if (code == Invalid.code) Invalid
    else Absent

  case object Absent extends ModifierSemanticValidity {
    override val code: Byte = 0
  }

  case object Unknown extends ModifierSemanticValidity {
    override val code: Byte = 1
  }

  case object Valid extends ModifierSemanticValidity {
    override val code: Byte = 2
  }

  case object Invalid extends ModifierSemanticValidity {
    override val code: Byte = 3
  }

}
