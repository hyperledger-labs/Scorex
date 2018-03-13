package spv

import commons.ExamplesCommonGenerators
import examples.spv._
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.ModifierId
import scorex.core.utils.ByteBoxer
import supertagged.tag

trait SPVGenerators extends ExamplesCommonGenerators {

  val blockHeaderGen: Gen[Header] = for {
    parentId: ModifierId <- modifierIdGen
    innerchainLinks: Seq[Array[Byte]] <- Gen.listOf(genBytesList(Constants.hashfn.DigestSize)).map(_.take(128))
    txRoot: Array[Byte] <- genBytesList(Constants.hashfn.DigestSize)
    stateRoot: Array[Byte] <- genBytesList(Constants.hashfn.DigestSize)
    timestamp: Long <- Arbitrary.arbitrary[Long].map(t => Math.abs(t))
    powNonce: Int <- Arbitrary.arbitrary[Int]
  } yield Header(ByteBoxer[ModifierId](tag[ModifierId](parentId)), innerchainLinks, stateRoot, txRoot, timestamp, powNonce)

}
