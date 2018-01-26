package examples

import supertagged.TaggedType

package object commons {

  object Value extends TaggedType[Long]
  object Nonce extends TaggedType[Long]

  type Value = Value.Type
  type Nonce = Nonce.Type
}
