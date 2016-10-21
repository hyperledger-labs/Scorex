package io.scalac.elm

import scala.collection.mutable
import scala.reflect.ClassTag

package object util {
  implicit class ByteKey(val array: Array[Byte]) extends mutable.WrappedArray[Byte] {
    def elemTag: ClassTag[Byte] = ClassTag.Byte
    def length: Int = array.length
    def apply(index: Int): Byte = array(index)
    def update(index: Int, elem: Byte) { array(index) = elem }

    def key: ByteKey = this
  }
}
