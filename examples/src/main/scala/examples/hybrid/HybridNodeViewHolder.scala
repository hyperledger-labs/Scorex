package examples.hybrid

import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks._
import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import examples.hybrid.mempool.HMemPool
import examples.hybrid.mining.MiningSettings
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import examples.hybrid.wallet.HWallet
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.{NodeViewHolder, NodeViewModifier}
import scorex.crypto.encode.Base58


class HybridNodeViewHolder(settings: MiningSettings) extends NodeViewHolder[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridBlock] {
  override val networkChunkSize: Int = settings.networkChunkSize

  override type SI = HybridSyncInfo

  override type HIS = HybridHistory
  override type MS = HBoxStoredState
  override type VL = HWallet
  override type MP = HMemPool

  override val modifierCompanions: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] =
    Map(PosBlock.ModifierTypeId -> PosBlockCompanion, PowBlock.ModifierTypeId -> PowBlockCompanion)

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    reason.printStackTrace()
    System.exit(100) // this actor shouldn't be restarted at all so kill the whole app if that happened
  }

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (HIS, MS, VL, MP) = {
    val GenesisAccountsNum = 50
    val GenesisBalance = 100000000L

    //propositions with wallet seed genesisoo, genesiso1, ..., genesis48, genesis49
    val icoMembers: IndexedSeq[PublicKey25519Proposition] = IndexedSeq("G1v4mmwprVejf4j72wdx9Yf3ghcKN3Y7a2dpBYJ5JL38", "G2r7XqSjVSXveqG2Cwa1VGEgCLrvtEyqBpV87kMYqD3q", "AxWRx5oHGBp8FkQy3nj8hfLmRo2Fj4RTYYMcVW2s6vcW", "8iturbPrAbH4GRH3LiQeb9FvVyFCXBXanbsZHMfgvPHP", "3Fyd7y2mpA3NP1FrAW1qeXQ7T6ErLne9HWtT5KssJCvq", "4GdjVmFpWruzBzH79j69XmVRTYB5zYRrySj8nU59DiPj", "Aucwaus1DS3RRRbKQb7nEswase6VEQTDh7WHKqsWGphn", "8Thi35a6GpWA5dfbM6Rv4JcWpz5MsXSm19L1mBbHzw5t", "1MUe6h4Wov2JR66W3gR5YiuceYpfxFkSW2NMmUEhmqS", "CYrSEhzSLaZ5aR8Peye5vo3jmVB96XTawqYg2xq6aSvm", "9hiEguDpAHHczbXUQoAtrHx9Rto28z77DwSN8TVF1YaM", "BSfEQdba2zJon6sUaJmwhkPmkEcqNZF2SJDUqoRpkn8x", "GyWK7aieLj4Ea3xCwMqqyWeMaJLF4F1otgbpGbpK8rej", "4HJiqbqzDNPS1Qq8rNdN3NkbMYaNk8nsnSkVBaxuaFe3", "7Vxbq38SzYkyvpiiViWtx4hKUV4JVZXKv58B36Lu2yEB", "5EBtrg1GZWnVNvkxqPoaj9UoNWin4d7ejB4qeUjqTx11", "5HGQrnKgYmzGnoF6znnJMxTmQbnd783wMLie4G1m9Fn2", "3Znmdmac1ymwyV9tXPACLGSSLDF5hn2NRwEy81sTKZk4", "8jg1fy3zj1ddp5Byuc4gv9bfV1osNTNE1fCSegiaMhZJ", "CWGRH6StA7gqYzcYSBv4ZZ9aTjCz5uLeNdfxxpFZUgbG", "Fo9f9UUMkJxhw3CqoYUbBzeaPF4e4fqDyob27yGbGgJo", "DSgKgg2mTJo3kGTJc7Hy7yT9mx1QjgzAEFpci66kTr3G", "DHwUk7swkVdxwoHiJ3fCLQi1n2YSD2QMonRQtMsWCtSy", "3LpcdULY5HDmTjv3sQe5zWtmaBfxx79AwvugsHuwK18i", "G29cfw8aPRDCmJq4nVZk7qm4Gj4uKzr4S9JYhQEzasZr", "EiSUZhacKc4DpKwaWN9UDLHBHy9ZA8ZiKm4piWwvZSms", "8jjPLJEFja5dgXCuiKD78CZM31EW9TDTFtCTKkY1eWvg", "H1RVppjaBrsPaaFTrfH4HzvkbLpgR4PdVQGepdzrjcHC", "FXSvauKJA7cAoNELX9KNFHHnoQoHpDhDKEdfE9BCzV12", "GuVZGWVs7gDT7vL4ZaL4ruMwfyMmysgbdb124BYhaRoo", "Gn933Kbxp1WWC9RmHLebRLcnbVDAw1gUppXQnHBwxz7p", "8Huy8S4PQTVepEvtEFApwWP1ZjW4T6rnKPTWfM5UjZpw", "AMQFfo1EUDnnKpxM2ZWRkj9fg4D3CcsgpD21HxQfPV3J", "EcBCHh75xQ6ikdAQRzc2PaJFp3amFMnhM18z9QTGyjCb", "2Thoo884GZxF8a5Dn62kkYUiWsMNgDQftR5WDFQsrLpM", "E7XjU1od4qMm8boDcnxAew1p6DeBAkP8Y7za3v4QjUoK", "5fyzxa8eMACPDqah34YLnKfopq8dUtuVG7kZW4U29vt7", "7RJtpNMDZPAuibHC2jhU9uJC61dSS7CPoYehMKYSWYgn", "JjTuxyzMYsTK4jNQATznT3qYou4po5yW7NXNBPowTxk", "7S6Gr3j8tJqUMue9NrJjK5WRKadJzNvbVy5UQs1HHNuT", "7XaGJa2adfEhER1wxoXJvbFh6rS8o2gDR3LowbL8ehJv", "ERxUGi9S6xQ1MTyDsBAm6uNUsK7hagZeg6TRwt6Y4tjn", "EAyThb6H4paoGq7idPosQw55VFRtoDW3ce5RWpm31bWx", "Cki6o1p5EE8Q8WtEmJsCKh1ZqKYrmNfSZWfxvBK4FUuy", "CovDAXET88nYMYM8MSnYUXVneMX6GJPHE9KpzR96nN7n", "CNh7p8uQZ26ybmFwd46FvXhePN3bcZVAf6tegUkXK5Cm", "A4fSXF1eJJChcbjCKXk1WDQrP4Lnnoz7vrpNueFi6A8L", "D1FHvbkdExzQ5hXBm69NZVzi6gRGECXTEwTWFLdXcWCS", "BKU8JQ2rr8TY7TPtyaqWdQPPhDAqqhos3uqUc2Mud6FK", "GVc89c4nEKmFhQAy9aZU8g98CKHH2eiYx7oNXXKfXFxV").map(s => PublicKey25519Proposition(Base58.decode(s).get))

    val genesisAccount = PrivateKey25519Companion.generateKeys("genesis".getBytes)
    val genesisAccountPriv = genesisAccount._1
    val powGenesis = PowBlock(settings.GenesisParentId, settings.GenesisParentId, 1481110008516L, 38,
      0, Array.fill(32)(0: Byte), genesisAccount._2, Seq())


    val genesisTxs = Seq(SimpleBoxTransaction(
      IndexedSeq(genesisAccountPriv -> 0),
      icoMembers.map(_ -> GenesisBalance),
      0L,
      0L))
    log.debug(s"Initialize state with transaction ${genesisTxs.head} with boxes ${genesisTxs.head.newBoxes}")
    assert(icoMembers.length == GenesisAccountsNum)
    assert(Base58.encode(genesisTxs.head.id) == "EfUShU2YhAJrJAotFZSRzYyUkgE7tVV87EQgepDFf3zN", Base58.encode(genesisTxs.head.id))

    val genesisBox = PublicKey25519NoncedBox(genesisAccountPriv.publicImage, 0, GenesisBalance)
    val attachment = "genesis attachment".getBytes
    val posGenesis = PosBlock.create(powGenesis.id, 0, genesisTxs, genesisBox, attachment, genesisAccountPriv)

    var history = HybridHistory.readOrGenerate(settings)
    history = history.append(powGenesis).get._1
    history = history.append(posGenesis).get._1

    val gs = HBoxStoredState.genesisState(settings, Seq(posGenesis, powGenesis))
    val gw = HWallet.genesisWallet(settings, Seq(posGenesis, powGenesis))
    assert(!Base58.encode(settings.walletSeed).startsWith("genesis") || gw.boxes().map(_.box.value).sum >= GenesisBalance)

    gw.boxes().foreach(b => assert(gs.closedBox(b.box.id).isDefined))

    (history, gs, gw, HMemPool.emptyPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  override def restoreState(): Option[(HIS, MS, VL, MP)] = {
    if (HWallet.exists(settings)) {
      Some((
        HybridHistory.readOrGenerate(settings),
        HBoxStoredState.readOrGenerate(settings),
        HWallet.readOrGenerate(settings),
        HMemPool.emptyPool))
    } else None
  }
}