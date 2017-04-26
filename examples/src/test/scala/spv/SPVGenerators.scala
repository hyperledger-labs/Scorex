package spv

import commons.ExamplesCommonGenerators
import examples.spv.{Header, _}
import org.scalacheck.{Arbitrary, Gen}

trait SPVGenerators extends ExamplesCommonGenerators {

  val blockHeaderGen: Gen[Header] = for {
    parentId: Array[Byte] <- genBytesList(Constants.hashfn.DigestSize)
    innerchainLinks: Seq[Array[Byte]] <- Gen.listOf(genBytesList(Constants.hashfn.DigestSize)).map(_.take(128))
    txRoot: Array[Byte] <- genBytesList(Constants.hashfn.DigestSize)
    stateRoot: Array[Byte] <- genBytesList(Constants.hashfn.DigestSize)
    timestamp: Long <- Arbitrary.arbitrary[Long].map(t => Math.abs(t))
    powNonce: Int <- Arbitrary.arbitrary[Int]
  } yield Header(parentId, innerchainLinks, stateRoot, txRoot, timestamp, powNonce)

}
