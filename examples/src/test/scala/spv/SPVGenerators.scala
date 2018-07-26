package spv

import commons.ExamplesCommonGenerators
import examples.spv._
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.ModifierId

trait SPVGenerators extends ExamplesCommonGenerators {

  val blockHeaderGen: Gen[Header] = for {
    parentId: ModifierId <- modifierIdGen
    innerchainLinks: Seq[ModifierId] <- Gen.listOf(modifierIdGen).map(_.take(128))
    txRoot: Array[Byte] <- genBytes(Constants.hashfn.DigestSize)
    stateRoot: Array[Byte] <- genBytes(Constants.hashfn.DigestSize)
    timestamp: Long <- Arbitrary.arbitrary[Long].map(t => Math.abs(t))
    powNonce: Int <- Arbitrary.arbitrary[Int]
  } yield Header(parentId, innerchainLinks, stateRoot, txRoot, timestamp, powNonce)

}
