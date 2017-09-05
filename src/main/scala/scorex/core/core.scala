package scorex

import supertagged.TaggedType

package object core {

  //TODO implement ModifierTypeId as a trait
  object ModifierTypeId extends TaggedType[Byte]

  object ModifierId extends TaggedType[Array[Byte]]

  object VersionTag extends TaggedType[Array[Byte]]

  type ModifierTypeId = ModifierTypeId.Type
  type ModifierId = ModifierId.Type
  type VersionTag = VersionTag.Type

}
