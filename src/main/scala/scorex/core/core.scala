package scorex

import scorex.core.ModifierId
import scorex.core.network.message.BasicMsgDataTypes.InvData
import scorex.core.utils.ScorexEncoder
import scorex.crypto.encode.{Base16, BytesEncoder}
import supertagged.TaggedType

import scala.language.implicitConversions

package object core {

  //TODO implement ModifierTypeId as a trait
  object ModifierTypeId extends TaggedType[Byte]

  object ModifierId extends TaggedType[String]

  object VersionTag extends TaggedType[String]

  type ModifierTypeId = ModifierTypeId.Type

  type ModifierId = ModifierId.Type

  type VersionTag = VersionTag.Type

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)])
                 (implicit encoder: ScorexEncoder): String = (ids.headOption, ids.lastOption) match {
    case (Some(f), Some(l)) if f._2 == l._2 =>
      s"[(${f._1},${encoder.encode(f._2)})]"
    case (Some(f), Some(l)) =>
      s"[(${f._1},${encoder.encode(f._2)})..(${l._1},${encoder.encode(l._2)})]"
    case _ =>
      "[]"
  }

  def idsToString(modifierType: ModifierTypeId, ids: Seq[ModifierId])(implicit encoder: ScorexEncoder): String = {
    idsToString(ids.map(id => (modifierType, id)))
  }

  def idsToString(invData: InvData)(implicit encoder: ScorexEncoder): String = idsToString(invData._1, invData._2)

  def bytesToId(bytes: Array[Byte]): ModifierId = ModifierId @@ Base16.encode(bytes)

  def idToBytes(id: ModifierId): Array[Byte] = Base16.decode(id).get

  def versionToBytes(id: VersionTag): Array[Byte] = Base16.decode(id).get

}
