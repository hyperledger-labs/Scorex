package io.scalac.elm.serialization

import scorex.core.{NodeViewModifier, NodeViewModifierCompanion}

trait Serialization[T <: Serializable with NodeViewModifier] extends NodeViewModifierCompanion[T] with ByteSerialization[T] with JsonSerialization[T]
