package hybrid

import examples.hybrid.blocks.PowBlock
import examples.hybrid.history.{HistoryStorage, HybridHistory}
import examples.hybrid.mining.{HybridMiningSettings, HybridSettings}
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.signatures.PublicKey

trait HistoryGenerators { this: StoreGenerators =>

  def settings: HybridSettings

  private val historyTimestamp = 1478164225796L
  private val historyNonce = -308545845552064644L
  private val historyBrothersCount = 0
  private val historyBrothersHash =  Array.fill(32)(0: Byte)
  private val historyBrothers = Seq.empty
  private val historyProposition = PublicKey25519Proposition(PublicKey @@ scorex.utils.Random.randomBytes(32))

  private val genesisBlock = PowBlock(
    settings.mining.GenesisParentId,
    settings.mining.GenesisParentId,
    historyTimestamp,
    historyNonce,
    historyBrothersCount,
    historyBrothersHash,
    historyProposition,
    historyBrothers)

  val historyGen: Gen[HybridHistory] = lsmStoreGen.map { blockStorage =>
    val storage = new HistoryStorage(blockStorage, settings.mining)
    //we don't care about validation here
    val validators = Seq()
    var history = new HybridHistory(storage, settings.mining, validators, None)
    history = history.append(genesisBlock).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }
}
