package hybrid

import akka.actor.{ActorRef, ActorSystem, Props}
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.curvepos.{Nonce, Value}
import examples.hybrid.HybridNodeViewHolder
import examples.hybrid.blocks._
import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import scorex.core.consensus.SyncInfo
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.PublicKey

trait NodeViewHolderGenerators { this: ModifierGenerators with StateGenerators with HistoryGenerators with Settings =>

  type P = PublicKey25519Proposition
  type TX = SimpleBoxTransaction
  type PM = HybridBlock
  type SI = SyncInfo

  type NODE = HybridNodeViewHolder
  type ST = HBoxStoredState
  type HT = HybridHistory

  class NodeViewHolderForTests extends HybridNodeViewHolder(settings) {
    import NodeViewHolderForTests._

    override protected def genesisState: (HIS, MS, VL, MP) = genesisStateHardcoded

  }

  object NodeViewHolderForTests extends ScorexLogging {

    type SI = HybridSyncInfo

    type HIS = HybridHistory
    type MS = HBoxStoredState
    type VL = HWallet
    type MP = SimpleBoxTransactionMemPool

    def props(): Props = Props(new NodeViewHolderForTests)

    def genesisStateHardcoded: (HIS, MS, VL, MP) = {
      val GenesisAccountsNum = 50
      val GenesisBalance = Value @@ 100000000L

      //propositions with wallet seed genesisoo, genesiso1, ..., genesis48, genesis49
      val icoMembers: IndexedSeq[PublicKey25519Proposition] = IndexedSeq("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ", "7BDhJv6Wh2MekgJLvQ98ot9xiw5x3N4b3KipURdrW8Ge", "Ei8oY3eg5vM26QUBhyFiAdPN1C23RJEV9irrykNmSAFV", "8LNhm5QagL88sWggvJKGDiZ5bBCG4ajV7R6vAKz4czA9", "EakiCSw1rfmL5DFTPNmSJZEEAEGtTp3DN12wVPJVsURS", "AEQ8bZRuAxAp8DV9VZnTrSudGPdNyzY2HXjPBCGy8igf", "DSL6bvb6j1v6SnvKjqc6fJWdsRjZ85YboH8FkzonUPiT", "419sTmWKAXb5526naQ93xJZL4YAYtpVkbLmzMb6k5X9m", "GydWCS1GwExoDNuEiW6fBLYr7cs4vwdLpk1kzDeKHq6A", "G8xVDYow1YcSb4cuAHwcpYSEKxFpYwC9GqYChMvbCWn5", "9E4F53GSXMPqwuPWEVoUQe9B1z4A8v9Y6tAQdKK779km", "5XtHBDxXCudA38FJnoWm1BVG8aV67AiQKnPuwYbWZCb3", "8Sp3v5vtYtkM9Z2K2B7PuZbWmWQE9bfiUFCvkmsdauGj", "8XTUXeLiHPbMNXedWQh5xHQtq4xUHU3pZZGqRQzC2eyj", "ftqJXjSXrWQXmumNVVaRiNB7TZuCy4GCvz9V4GJGhAv", "GMAYWvbBmssCr55m9bcq8cKzfczSKKxidtVrukBM1KFN", "3nFprwUuqGH9BpvJMQeCb5AwHdaXuxKin1WSxWc9PTkY", "HfYNA96cGebFGgAhGUbxvRJYyLFchQJZpJTQMXztE6gZ", "EPbo8xRWARg2znJAqevKnQMskxnemmCdimPiVFhr8eLd", "4pygr1SPEe5KbU1R8XgMmYaW7YfTH818wd113mF6bhsP", "52gwahUytUXv7wfKs4j6YeKeepc38sYsUi4jp4z4jVym", "Hi3Q1ZQbD2zztq6ajm5yUKfFccxmj3yZn79GUjhFvPSW", "G1yK5iwPQKNXnqU4Drg83et3gKhRW5CogqiekKEYDcrt", "Hf8XcEAVMCiWbu376rGS48FhwH5NgteivfsTsvX1XpbA", "3FAskwxrbqiX2KGEnFPuD3z89aubJvvdxZTKHCrMFjxQ", "GgahaaNBaHRnyUtvEu3k7N5BnW3dvhVCXyxMP6uijdhh", "7R9waVeAKuHKNQY5uTYBp6zNLNo6wSDvj9XfQCyRWmDF", "E4AoFDANgDFL83gTS6A7kjWbLmqWcPr6DqEgMG7cqU18", "AEkuiLFdudYmUwZ9dSa64rakqUgJZf6pKFFwwm6CZFQz", "3QzGZvvTQbcUdhd5BL9ofEK3GdzbmqUnYA1pYTAdVY44", "EjpGvdZETt3SuZpcuwKvZS4jgWCockDHzFQLoeYNW4R", "C85c1uMAiHKDgcqxF6EaiCGQyWgQEYATbpo8M7XEnx3R", "8V5y1CSC1gCGD1jai3ns5FJNW7tAzf7BGd4iwmBv7V44", "CJ9udTDT61ckSHMd6YNpjeNdsN2fGwmJ6Ry6YERXmGa7", "7eboeRCeeBCFwtzPtB4vKPnaYMPL52BjfiEpqSRWfkgx", "E3JJCTMouTys5BSwFyHTV3Ht55mYWfNUAverrNaVo4jE", "9PLHPwnHyA5jf6GPGRjJt7HNd93rw4gWTBi7LBNL4Wwt", "2YM2FQ4HfMiV3LFkiwop2xFznbPVEHbhahVvcrhfZtXq", "3oTzYXjwdr684FUzaJEVVuXBztysNgR8M8iV9QykaM9C", "J6bgGpwDMqKFrde2mpdS6dasRyn9WFV6jKgWAkHSN91q", "4wtQpa1BVgAt9CA4FUuHZHCYGBYtvudPqa1sAddfAPii", "DaSXwzkAU2WfH39zxMfuXpExsVfKk6JzeYbdW9RLiXr4", "6BtXEZE6GcxtEtSLAHXkE3mkcTG1u8WuoQxZG7R8BR5X", "39Z9VaCAeqoWajHyku29argf7zmVqs2vVJM8zYe7YLXy", "7focbpSdsNNE4x9h7eyXSkvXE6dtxsoVyZMpTpuThLoH", "CBdnTL6C4A7nsacxCP3VL3TqUokEraFy49ckQ196KU46", "CfvbDC8dxGeLXzYhDpNpCF2Ar9Q5LKs8QrfcMYAV59Lt", "GFseSi5squ8GRRkj6RknbGj9Hyz82HxKkcn8NKW1e5CF", "FuTHJNKaPTneEYRkjKAC3MkSttvAC7NtBeb2uNGS8mg3", "5hhPGEFCZM2HL6DNKs8KvUZAH3wC47rvMXBGftw9CCA5").map(s => PublicKey25519Proposition(PublicKey @@ Base58.decode(s).get))

      val genesisAccount = PrivateKey25519Companion.generateKeys("genesis".getBytes)
      val genesisAccountPriv = genesisAccount._1
      val powGenesis = PowBlock(settings.GenesisParentId, settings.GenesisParentId, 1481110008516L, 38,
        0, Array.fill(32)(0: Byte), genesisAccount._2, Seq())


      val genesisTxs = Seq(SimpleBoxTransaction(
        IndexedSeq(genesisAccountPriv -> Nonce @@ 0L),
        icoMembers.map(_ -> GenesisBalance),
        0L,
        0L))
      log.debug(s"Initialize state with transaction ${genesisTxs.head} with boxes ${genesisTxs.head.newBoxes}")
      assert(icoMembers.length == GenesisAccountsNum)
      assert(Base58.encode(genesisTxs.head.id) == "EKuWxCuUAg9XgVWKxsnehP9FLsF3zPSyn9yczqeBHD8S", Base58.encode(genesisTxs.head.id))

      val genesisBox = PublicKey25519NoncedBox(genesisAccountPriv.publicImage, Nonce @@ 0L, GenesisBalance)
      val attachment = "genesis attachment".getBytes
      val posGenesis = PosBlock.create(powGenesis.id, 0, genesisTxs, genesisBox, attachment, genesisAccountPriv)

      var history = HybridHistory.readOrGenerate(settings)
      history = history.append(powGenesis).get._1
      history = history.append(posGenesis).get._1

      val gs = HBoxStoredState.genesisState(settings, Seq(posGenesis, powGenesis))
      val gw = HWallet.genesisWallet(settings, Seq(posGenesis, powGenesis))
      assert(!Base58.encode(settings.walletSeed).startsWith("genesis") || gw.boxes().map(_.box.value.toLong).sum >= GenesisBalance)

      assert(gw.boxes().forall(b => gs.closedBox(b.box.id).isDefined))

      (history, gs, gw, SimpleBoxTransactionMemPool.emptyPool)
    }
  }

  def nodeViewHolder(implicit system: ActorSystem): (ActorRef, PM, ST, HT) = {
    val ref = system.actorOf(NodeViewHolderForTests.props())
    val (h, s, _, _) = NodeViewHolderForTests.genesisStateHardcoded
    val m = totallyValidModifier(h, s)
    (ref, m, s, h)
  }
}