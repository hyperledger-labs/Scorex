package examples.spv

import scorex.core.serialization.Serializer

import scala.util.Try

case class MKZProof(m: Int, k: Int, prefixProofs: Seq[Seq[Header]], suffix: Seq[Header]) {
  lazy val valid: Try[Unit] = Try {
    require(suffix.length == k, "Wrong suffix length")

    suffix.foldRight(Array[Byte]()) { (a, b) =>
      if (b.nonEmpty) require(b sameElements a.id)
      a.parentId
    }
  }
}

object MKZProofSerializer extends Serializer[MKZProof] {
  override def toBytes(obj: MKZProof): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[MKZProof] = ???
}