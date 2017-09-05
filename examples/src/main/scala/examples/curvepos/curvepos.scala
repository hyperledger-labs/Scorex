package examples

import supertagged.TaggedType

package object curvepos {

  object GenerationSignature extends TaggedType[Array[Byte]]

  object BaseTarget extends TaggedType[Long]

  type GenerationSignature = GenerationSignature.Type

  type BaseTarget = BaseTarget.Type

}
