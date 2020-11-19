package hybrid.state

import examples.commons.{Nonce, PublicKey25519NoncedBox, SimpleBoxTransaction, Value}
import examples.hybrid.state.HBoxStoredState
import hybrid.HybridGenerators
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.{Signature25519, Signature25519Serializer}
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.util.encode.Base58
import scorex.crypto.hash.Sha256
import scorex.crypto.signatures.{PublicKey, Signature}

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
class SimpleBoxTransactionSpecification extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with HybridGenerators {


  property("Transaction boxes are deterministic") {
    val GenesisAccountsNum = 10
    val GenesisBalance = Value @@ 100000L

    val icoMembers = (1 to 10) map (i => PublicKey25519Proposition(PublicKey @@ Sha256(i.toString)))
    icoMembers.map(_.address).mkString(",") shouldBe "016b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7875b4b658ee50f,01d4735e3a265e16eee03f59718b9b5d03019c07d8b6c51f90da3a666eec13ab35aca2afcd,014e07408562bedb8b60ce05c1decfe3ad16b72230967de01f640b7e4729b49fce65d47374,014b227777d4dd1fc61c6f884f48641d02b4d121d3fd328cb08b5531fcacdabf8ae325532b,01ef2d127de37b942baad06145e54b0c619a1f22327b2ebbcfbec78f5564afe39de3a3a62c,01e7f6c011776e8db7cd330b54174fd76f7d0216b612387a5ffcfb81e6f0919683f1c991e5,017902699be42c8a8e46fbbb4501726517e86b22c56a189f7625a6da49081b245190431363,012c624232cdd221771294dfbb310aca000a0df6ac8b66b696d90ef06fdefb64a3f41c07f5,0119581e27de7ced00ff1ce50b2047e7a567c76b1cbaebabe5ef03f7c3017bb5b7a7c7c153,014a44dc15364204a80fe80e9039455cc1608281820fe2b24f1e5233ade6af1dd5d5ab8000"

    val genesisAccount = PrivateKey25519Companion.generateKeys("genesis".getBytes)._1
    val tx = SimpleBoxTransaction(IndexedSeq(genesisAccount -> Nonce @@ 0L), icoMembers.map(_ -> GenesisBalance), 0L, 0L)
    tx.newBoxes.toSeq shouldBe
      Vector(
        PublicKey25519NoncedBox(PublicKey25519Proposition(PublicKey @@ Base58.decode("8EjkXVSTxMFjCvNNsTo8RBMDEVQmk7gYkW4SCDuvdsBG").get), Nonce @@ 5440538097828856759L, Value @@ 100000L),
        PublicKey25519NoncedBox(PublicKey25519Proposition(PublicKey @@ Base58.decode("FJKTv1un7qsnyKdwKez7B67JJp3oCU5ntCVXcRsWEjtg").get), Nonce @@ -1616008834867344494L, Value @@ 100000L),
        PublicKey25519NoncedBox(PublicKey25519Proposition(PublicKey @@ Base58.decode("6FbDRScGruVdATaNWzD51xJkTfYCVwxSZDb7gzqCLzwf").get), Nonce @@ 8479754679609603533L, Value @@ 100000L),
        PublicKey25519NoncedBox(PublicKey25519Proposition(PublicKey @@ Base58.decode("64J4UGtfZqfnvxWCwU1aSMN62xqxLiS61iEPuD9JWxAm").get), Nonce @@ -4288295896200546936L, Value @@ 100000L),
        PublicKey25519NoncedBox(PublicKey25519Proposition(PublicKey @@ Base58.decode("H6eJWWkvryDNAeocEv5VejKHhG1sR8kWt4jqPmks2TDN").get), Nonce @@ -6878427772037923352L, Value @@ 100000L),
        PublicKey25519NoncedBox(PublicKey25519Proposition(PublicKey @@ Base58.decode("GcVQWfSWUKoPuxbhoqx18hD4JKs2L1cvVvJZFzXKWaQ2").get), Nonce @@ 8815511758437892756L, Value @@ 100000L),
        PublicKey25519NoncedBox(PublicKey25519Proposition(PublicKey @@ Base58.decode("99NTyZ796bpvwLLhMmsfwo8J3Wu3rUioUQsHE9CSYQKz").get), Nonce @@ -7463379991470698374L, Value @@ 100000L),
        PublicKey25519NoncedBox(PublicKey25519Proposition(PublicKey @@ Base58.decode("3zFqfiRPEoshgaZY7qCcSk6mihDhgnGodBDgqP92stci").get), Nonce @@ -3238799622036337810L, Value @@ 100000L),
        PublicKey25519NoncedBox(PublicKey25519Proposition(PublicKey @@ Base58.decode("2hw8D3T2Jrf7QZ9k53gDxDWjrnCXLLtDv1oonKGzKw74").get), Nonce @@ -6037688402013706435L, Value @@ 100000L),
        PublicKey25519NoncedBox(PublicKey25519Proposition(PublicKey @@ Base58.decode("5zv52oTnDQas6WWnftRRhtZiudNaNTJ72WZWfDjRtKCQ").get), Nonce @@ -8390758926615640787L, Value @@ 100000L)
      )
  }

  property("Generated transaction is valid") {
    forAll(simpleBoxTransactionGen) { tx =>
      HBoxStoredState.semanticValidity(tx).isSuccess shouldBe true
    }
  }

  property("Transaction with modified signature is invalid") {
    forAll(simpleBoxTransactionGen) { tx =>
      val serializer =Signature25519Serializer
      val wrongSig = Signature @@ ((serializer.toBytes(tx.signatures.head).head + 1).toByte +: serializer.toBytes(tx.signatures.head).tail)
      val wrongSigs = (Signature25519(wrongSig) +: tx.signatures.tail).toIndexedSeq
      HBoxStoredState.semanticValidity(tx.copy(signatures = wrongSigs)).isSuccess shouldBe false
    }
  }

  property("Transaction with modified from is invalid") {
    forAll(simpleBoxTransactionGen) { tx =>
      val wrongFromPub = tx.from.map(p => (p._1, Nonce @@ (p._2 + 1)))
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
