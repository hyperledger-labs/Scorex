package scorex.core.consensus

sealed trait ModifierSemanticValidity
case object Absent extends ModifierSemanticValidity
case object Unknown extends ModifierSemanticValidity
case object Valid extends ModifierSemanticValidity
case object Invalid extends ModifierSemanticValidity
