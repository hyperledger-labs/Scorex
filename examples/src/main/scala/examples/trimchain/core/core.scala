package examples.trimchain

import supertagged.TaggedType

package object core {

  object StateRoot extends TaggedType[Array[Byte]]

  object TransactionsRoot extends TaggedType[Array[Byte]]

  type StateRoot = StateRoot.Type

  type TransactionsRoot = TransactionsRoot.Type

}
