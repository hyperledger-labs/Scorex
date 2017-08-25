package commons

import examples.commons.SimpleBoxTransaction
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.testkit.generators.CoreGenerators

trait ExamplesCommonGenerators extends CoreGenerators {
  lazy val pGen: Gen[(PublicKey25519Proposition, Long)] = for {
    prop <- propositionGen
    long <- positiveLongGen
  } yield (prop, long)

  lazy val privGen: Gen[(PrivateKey25519, Long)] = for {
    prop <- key25519Gen.map(_._1)
    long <- positiveLongGen
  } yield (prop, long)

  lazy val simpleBoxTransactionGen: Gen[SimpleBoxTransaction] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    from: IndexedSeq[(PrivateKey25519, Long)] <- smallInt.flatMap(i => Gen.listOfN(i + 1, privGen).map(_.toIndexedSeq))
    to: IndexedSeq[(PublicKey25519Proposition, Long)] <- smallInt.flatMap(i => Gen.listOfN(i, pGen).map(_.toIndexedSeq))
  } yield SimpleBoxTransaction(from, to, fee, timestamp)

  lazy val simpleBoxTransactionsGen: Gen[List[SimpleBoxTransaction]] = for {
    txs <- smallInt.flatMap(i => Gen.listOfN(i, simpleBoxTransactionGen))
  } yield txs

  def simpleBoxTransactionGenCustomMakeBoxes (toBoxes: IndexedSeq[(PublicKey25519Proposition, Long)]): Gen[SimpleBoxTransaction] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    from: IndexedSeq[(PrivateKey25519, Long)] <- Gen.choose(1,1).flatMap(i => Gen.listOfN(i + 1, privGen).map(_.toIndexedSeq))
    to = toBoxes
  } yield SimpleBoxTransaction(from, to, fee, timestamp)

  def simpleBoxTransactionGenCustomUseBoxes (fromBoxes: IndexedSeq[(PrivateKey25519, Long)]): Gen[SimpleBoxTransaction] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    from = fromBoxes
    to: IndexedSeq[(PublicKey25519Proposition, Long)] <- Gen.choose(1,1).flatMap(i => Gen.listOfN(i, pGen).map(_.toIndexedSeq))
  } yield SimpleBoxTransaction(from, to, fee, timestamp)


}
