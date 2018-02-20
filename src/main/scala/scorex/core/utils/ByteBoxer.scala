package scorex.core.utils

import scorex.core
import scorex.core.ModifierId
import supertagged.{@@, tag}

case class ByteBoxer[T](arr: Array[Byte] @@ T)