package scorex

import scorex.core.network.message.InvData
import scorex.core.utils.ScorexEncoder
import scorex.util.encode.Base16
import supertagged.TaggedType

package object core {

  //TODO implement ModifierTypeId as a trait
  object ModifierTypeId extends TaggedType[Byte]

  @deprecated("use `scorex.util.ModifierId`", "")
  type ModifierId = scorex.util.ModifierId.Type

  @deprecated("use `scorex.util.ModifierId`", "")
  val ModifierId: scorex.util.ModifierId.type = scorex.util.ModifierId

  object VersionTag extends TaggedType[String]

  type ModifierTypeId = ModifierTypeId.Type

  type VersionTag = VersionTag.Type

  def idsToString(ids: Seq[(ModifierTypeId, util.ModifierId)])(implicit enc: ScorexEncoder): String = {
    List(ids.headOption, ids.lastOption)
      .flatten
      .map { case (typeId, id) => s"($typeId,${enc.encodeId(id)})" }
      .mkString("[", "..", "]")
  }

  def idsToString(modifierType: ModifierTypeId, ids: Seq[util.ModifierId])(implicit encoder: ScorexEncoder): String = {
    idsToString(ids.map(id => (modifierType, id)))
  }

  def idsToString(invData: InvData)(implicit encoder: ScorexEncoder): String = idsToString(invData.typeId, invData.ids)

  def bytesToId: Array[Byte] => util.ModifierId = scorex.util.bytesToId

  def idToBytes: util.ModifierId => Array[Byte] = scorex.util.idToBytes

  def bytesToVersion(bytes: Array[Byte]): VersionTag = VersionTag @@ Base16.encode(bytes)

  def versionToBytes(id: VersionTag): Array[Byte] = Base16.decode(id).get

  def versionToId(version: VersionTag): util.ModifierId = util.ModifierId @@ version

  def idToVersion(id: util.ModifierId): VersionTag = VersionTag @@ id

}
