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
    override val code = 0.toByte
  }

  case object Unknown extends ModifierSemanticValidity {
    override val code = 1.toByte
  }

  case object Valid extends ModifierSemanticValidity {
    override val code = 2.toByte
  }

  case object Invalid extends ModifierSemanticValidity {
    override val code = 3.toByte
  }

}
