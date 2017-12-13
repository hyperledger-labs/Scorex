package scorex

import scorex.crypto.encode.Base58
import supertagged.TaggedType

package object core {

  //TODO implement ModifierTypeId as a trait
  object ModifierTypeId extends TaggedType[Byte]

  object ModifierId extends TaggedType[Array[Byte]]

  object VersionTag extends TaggedType[Array[Byte]]

  type ModifierTypeId = ModifierTypeId.Type

  type ModifierId = ModifierId.Type

  type VersionTag = VersionTag.Type

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)]): String = (ids.headOption, ids.lastOption) match {
    case (Some(f), Some(l)) if f._2 sameElements l._2 => s"[(${f._1},${Base58.encode(f._2)})]"
    case (Some(f), Some(l)) => s"[(${f._1},${Base58.encode(f._2)})..(${l._1},${Base58.encode(l._2)})]"
    case _ => "[]"
  }

  def idsToString(modifierType: ModifierTypeId, ids: Seq[ModifierId]): String = {
    idsToString(ids.map(id => (modifierType, id)))
  }

}
