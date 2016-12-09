package io.scalac.elm

import scorex.crypto.encode.Base58

import scala.collection.mutable
import scala.reflect.ClassTag

package object util {

  object ByteKey {
    def base58(s: String) = Base58.decode(s).get.key
  }

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

    def base58: String = Base58.encode(array)
  }
}
