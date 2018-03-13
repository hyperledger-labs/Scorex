package scorex.core.utils

import java.util

import supertagged.@@

case class ByteBoxer[T](arr: Array[Byte] @@ T) {

  override def equals(o: Any): Boolean =
    o.isInstanceOf[ByteBoxer[T]] &&
      util.Arrays.equals(arr, o.asInstanceOf[ByteBoxer[T]].arr)

  override def hashCode: Int = util.Arrays.hashCode(arr)


  override def toString: String = {
    getClass.getSimpleName + "[" + javax.xml.bind.DatatypeConverter.printHexBinary(arr) + "]"
  }
}