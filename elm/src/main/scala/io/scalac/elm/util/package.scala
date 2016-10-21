package io.scalac.elm

import scala.collection.mutable
import scala.reflect.ClassTag

package object util {

  /**
    * Arrays are not suitable for comparison, for example as keys in maps.
    * This utility provides convenient conversions to WrappedArray
    */
  implicit class ByteKey(val array: Array[Byte]) extends mutable.WrappedArray[Byte] {
    def elemTag: ClassTag[Byte] = ClassTag.Byte
    def length: Int = array.length
    def apply(index: Int): Byte = array(index)
    def update(index: Int, elem: Byte) { array(index) = elem }

    def key: ByteKey = this
  }
}
