package io.scalac.elm.serialization

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import scala.util.Try

trait ByteSerialization[T <: Serializable] {
  def bytes(m: T): Array[Byte] = {
    val bytesOut = new ByteArrayOutputStream()
    val objectOut = new ObjectOutputStream(bytesOut)
    objectOut.writeObject(m)
    objectOut.close()
    bytesOut.toByteArray
  }

  def parse(bytes: Array[Byte]): Try[T] = Try {
    val bytesIn = new ByteArrayInputStream(bytes)
    val objectIn = new ObjectInputStream(bytesIn)
    val result = objectIn.readObject()
    objectIn.close()
    result.asInstanceOf[T]
  }
}
