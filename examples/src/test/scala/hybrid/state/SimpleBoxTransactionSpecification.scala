package hybrid.state

import examples.commons.SimpleBoxTransaction
import examples.hybrid.state.HBoxStoredState
import hybrid.HybridGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.hash.Sha256

class SimpleBoxTransactionSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {


  property("Transaction boxes are deterministic") {
    val GenesisAccountsNum = 10
    val GenesisBalance = 100000L

    val icoMembers = (1 to 10) map (i => PublicKey25519Proposition(Sha256(i.toString)))
    icoMembers.map(_.address).mkString(",") shouldBe "3m6nhP4AZjFn5pgMd3PvH6PwHx23AG4tvpLCuu7Wt3hhAPssKc,4ZJwiEzpTHhvT6BMYZg1FUXysHkuBLRHb7FvXhZGx6HtsWZCeG,3Y7Ji8wrYZ12EPup6ky2mWEaNo1wTgUKVPJ84xaHwHqTC6LXoh,3WqPcQ1w1HEaEDvHpnnqqYxJBzQGcf5gT5G5CrsXFL7UX4SA2N,4m5cG82kztD9bZVf1Tc1Ni1uvHobpKYuAUyxNSnDm7WLGCZvZh,4huPANjYcqcdRm99tsCw29JqFnHMTJZsQjoufRQTEDPPoWmPSt,3s3CauhVba81UefEuuaNqRqGLEV9jCZJpvLFg5dJdu29TivRZk,3HHuHxBf2eXmbUcGuFCx3dU6Wp7imeRiN5uz4rYDdQwsLwnwW4,38uZVfModMnCg5FSECtFiBE7Dbjmh7Tt1SgBD8gFTA1XDHxiqQ,3WTH7tB28nkbC9KFJTy8EBn1bWkxryiLKDnngeP9BYyuCik3aP"

    val genesisAccount = PrivateKey25519Companion.generateKeys("genesis".getBytes)._1
    val tx = SimpleBoxTransaction(IndexedSeq(genesisAccount -> 0), icoMembers.map(_ -> GenesisBalance), 0L, 0L)
    tx.newBoxes.toString() shouldBe "Vector(PublicKey25519NoncedBox(3m6nhP4AZjFn5pgMd3PvH6PwHx23AG4tvpLCuu7Wt3hhAPssKc,-6219502975712200872,100000), PublicKey25519NoncedBox(4ZJwiEzpTHhvT6BMYZg1FUXysHkuBLRHb7FvXhZGx6HtsWZCeG,2326174055960855030,100000), PublicKey25519NoncedBox(3Y7Ji8wrYZ12EPup6ky2mWEaNo1wTgUKVPJ84xaHwHqTC6LXoh,-2090466149357841238,100000), PublicKey25519NoncedBox(3WqPcQ1w1HEaEDvHpnnqqYxJBzQGcf5gT5G5CrsXFL7UX4SA2N,-4786344880748433993,100000), PublicKey25519NoncedBox(4m5cG82kztD9bZVf1Tc1Ni1uvHobpKYuAUyxNSnDm7WLGCZvZh,2879476891976400353,100000), PublicKey25519NoncedBox(4huPANjYcqcdRm99tsCw29JqFnHMTJZsQjoufRQTEDPPoWmPSt,4610029492489107892,100000), PublicKey25519NoncedBox(3s3CauhVba81UefEuuaNqRqGLEV9jCZJpvLFg5dJdu29TivRZk,416797087985622128,100000), PublicKey25519NoncedBox(3HHuHxBf2eXmbUcGuFCx3dU6Wp7imeRiN5uz4rYDdQwsLwnwW4,-8485818448745401936,100000), PublicKey25519NoncedBox(38uZVfModMnCg5FSECtFiBE7Dbjmh7Tt1SgBD8gFTA1XDHxiqQ,-4750873086163930339,100000), PublicKey25519NoncedBox(3WTH7tB28nkbC9KFJTy8EBn1bWkxryiLKDnngeP9BYyuCik3aP,1904873933279744536,100000))"

  }

  property("Generated transaction is valid") {
    forAll(simpleBoxTransactionGen) { tx =>
      HBoxStoredState.semanticValidity(tx).isSuccess shouldBe true
    }
  }

  property("Transaction with modified signature is invalid") {
    forAll(simpleBoxTransactionGen) { tx =>
      val wrongSig: Array[Byte] = (tx.signatures.head.bytes.head + 1).toByte +: tx.signatures.head.bytes.tail
      val wrongSigs = (Signature25519(wrongSig) +: tx.signatures.tail).toIndexedSeq
      HBoxStoredState.semanticValidity(tx.copy(signatures = wrongSigs)).isSuccess shouldBe false
    }
  }

  property("Transaction with modified from is invalid") {
    forAll(simpleBoxTransactionGen) { tx =>
      val wrongFromPub = tx.from.map(p => (p._1, p._2 + 1))
      HBoxStoredState.semanticValidity(tx.copy(from = wrongFromPub)).isSuccess shouldBe false
    }
  }

  property("Transaction with modified timestamp is invalid") {
    forAll(simpleBoxTransactionGen) { tx =>
      HBoxStoredState.semanticValidity(tx.copy(timestamp = tx.timestamp + 1)).isSuccess shouldBe false
    }
  }

  property("Transaction with modified fee is invalid") {
    forAll(simpleBoxTransactionGen) { tx =>
      HBoxStoredState.semanticValidity(tx.copy(fee = tx.fee + 1)).isSuccess shouldBe false
    }
  }

}
