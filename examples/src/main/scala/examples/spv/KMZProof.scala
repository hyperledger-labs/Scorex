package examples.spv

import scorex.core.serialization.Serializer

import scala.util.Try

case class KMZProof(m: Int, k: Int, prefixProofs: Seq[Seq[Header]], suffix: Seq[Header]) {
  lazy val valid: Try[Unit] = Try {
    require(suffix.length == k, "Wrong suffix length")

    suffix.foldRight(Array[Byte]()) { (a, b) =>
      if (b.nonEmpty) require(b sameElements a.id)
      a.parentId
    }
  }
}

object KMZProofSerializer extends Serializer[KMZProof] {
  override def toBytes(obj: KMZProof): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[KMZProof] = ???
}