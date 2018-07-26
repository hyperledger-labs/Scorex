package examples

import io.iohk.iodb.ByteArrayWrapper
import scorex.core._
import supertagged.TaggedType

package object commons {

  object Value extends TaggedType[Long]
  object Nonce extends TaggedType[Long]

  type Value = Value.Type
  type Nonce = Nonce.Type

  def idToBAW(id: ModifierId): ByteArrayWrapper = ByteArrayWrapper(idToBytes(id))

  def versionToBAW(id: VersionTag): ByteArrayWrapper = ByteArrayWrapper(versionToBytes(id))

}
