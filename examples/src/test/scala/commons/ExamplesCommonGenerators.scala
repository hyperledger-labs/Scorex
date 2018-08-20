package commons

import examples.commons.{Nonce, SimpleBoxTransaction, Value}
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.testkit.generators.CoreGenerators

trait ExamplesCommonGenerators extends CoreGenerators {
  lazy val nonceGen: Gen[Nonce] = positiveLongGen.map(a => Nonce @@ a)

  lazy val valueGen: Gen[Value] = positiveLongGen.map(a => Value @@ a)

  lazy val pGen: Gen[(PublicKey25519Proposition, Value)] = for {
    prop <- propositionGen
    long <- valueGen
  } yield (prop, long)

  lazy val privGen: Gen[(PrivateKey25519, Nonce)] = for {
    prop <- key25519Gen.map(_._1)
    long <- nonceGen
  } yield (prop, long)

  lazy val simpleBoxTransactionGen: Gen[SimpleBoxTransaction] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    from: IndexedSeq[(PrivateKey25519, Nonce)] <- smallInt.flatMap(i => Gen.listOfN(i + 1, privGen).map(_.toIndexedSeq))
    to: IndexedSeq[(PublicKey25519Proposition, Value)] <- smallInt.flatMap(i => Gen.listOfN(i, pGen).map(_.toIndexedSeq))
  } yield SimpleBoxTransaction(from, to, fee, timestamp)

  lazy val simpleBoxTransactionsGen: Gen[List[SimpleBoxTransaction]] = for {
    txs <- smallInt.flatMap(i => Gen.listOfN(i, simpleBoxTransactionGen))
  } yield txs

  def simpleBoxTransactionGenCustomMakeBoxes(toBoxes: IndexedSeq[(PublicKey25519Proposition, Value)]): Gen[SimpleBoxTransaction] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    from: IndexedSeq[(PrivateKey25519, Nonce)] <- Gen.choose(1, 1).flatMap(i => Gen.listOfN(i + 1, privGen).map(_.toIndexedSeq))
    to = toBoxes
  } yield SimpleBoxTransaction(from, to, fee, timestamp)

  def simpleBoxTransactionGenCustomUseBoxes(fromBoxes: IndexedSeq[(PrivateKey25519, Nonce)]): Gen[SimpleBoxTransaction] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    from = fromBoxes
    to: IndexedSeq[(PublicKey25519Proposition, Value)] <- Gen.choose(1, 1).flatMap(i => Gen.listOfN(i, pGen).map(_.toIndexedSeq))
  } yield SimpleBoxTransaction(from, to, fee, timestamp)
}
