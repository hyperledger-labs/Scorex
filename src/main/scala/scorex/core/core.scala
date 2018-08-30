package scorex

import scorex.core.network.message.BasicMsgDataTypes.InvData
import scorex.core.utils.ScorexEncoder
import scorex.util.encode.Base16
import supertagged.TaggedType

package object core {

  //TODO implement ModifierTypeId as a trait
  object ModifierTypeId extends TaggedType[Byte]

  type ModifierId = scorex.util.ModifierId.Type
  val ModifierId: scorex.util.ModifierId.type = scorex.util.ModifierId

  object VersionTag extends TaggedType[String]

  type ModifierTypeId = ModifierTypeId.Type

  type VersionTag = VersionTag.Type

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)])(implicit enc: ScorexEncoder): String = {
    List(ids.headOption, ids.lastOption)
      .flatten
      .map { case (typeId, id) => s"($typeId,${enc.encode(id)})" }
      .mkString("[", "..", "]")
  }

  def idsToString(modifierType: ModifierTypeId, ids: Seq[ModifierId])(implicit encoder: ScorexEncoder): String = {
    idsToString(ids.map(id => (modifierType, id)))
  }

  def idsToString(invData: InvData)(implicit encoder: ScorexEncoder): String = idsToString(invData._1, invData._2)

  def bytesToId: Array[Byte] => util.ModifierId = scorex.util.bytesToId

  def idToBytes: util.ModifierId => Array[Byte] = scorex.util.idToBytes

  def bytesToVersion(bytes: Array[Byte]): VersionTag = VersionTag @@ Base16.encode(bytes)

  def versionToBytes(id: VersionTag): Array[Byte] = Base16.decode(id).get

  def versionToId(version: VersionTag): ModifierId = ModifierId @@ version

  def idToVersion(id: ModifierId): VersionTag = VersionTag @@ id

}
