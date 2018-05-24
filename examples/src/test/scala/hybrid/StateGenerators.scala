package hybrid

import examples.commons.{Nonce, PublicKey25519NoncedBox, Value}
import examples.hybrid.state.HBoxStoredState
import io.iohk.iodb.LSMStore
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{BoxStateChanges, Insertion, PrivateKey25519Companion}
import scorex.testkit.generators.CoreGenerators

import scala.util.Random

trait StateGenerators extends StoreGenerators { this: HybridGenerators with CoreGenerators with StoreGenerators =>

  private val valueSeed = 5000000

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  val stateGen: Gen[HBoxStoredState] =
    for {
      dir <- tempDirGen
      keepVersions <- Gen.chooseNum(5, 20)
    } yield {
      def randomBox(): PublicKey25519NoncedBox = {
        val value: Value = Value @@ (Random.nextInt(valueSeed) + valueSeed).toLong
        val nonce: Nonce = Nonce @@ Random.nextLong()
        val keyPair = privKey(value)
        PublicKey25519NoncedBox(keyPair._2, nonce, value)
          .ensuring(box => PrivateKey25519Companion.owns(keyPair._1, box))
      }

      val store = new LSMStore(dir, keepVersions = keepVersions)
      val s0 = HBoxStoredState(store, versionTagGen.sample.get)
      val inserts = (1 to 10000)
        .map(_ => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](randomBox()))
      s0.applyChanges(BoxStateChanges(inserts), versionTagGen.sample.get).get
    }
}
