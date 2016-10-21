package io.scalac.elm.serialization

import scorex.core.NodeViewModifierCompanion

trait Serialization[T <: Serializable] extends NodeViewModifierCompanion[T] with ByteSerialization[T] with JsonSerialization[T]
