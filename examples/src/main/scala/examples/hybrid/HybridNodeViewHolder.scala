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
    val icoMembers: IndexedSeq[PublicKey25519Proposition] = IndexedSeq("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb","4GdjVmFpWruzBzH79j69XmVRTYB5zYRrySj8nU59DiPj","9hiEguDpAHHczbXUQoAtrHx9Rto28z77DwSN8TVF1YaM","2NXFabWwDNK5wyM8KXFoMvbNumxh82XNnutaioQKamsD","5dTeSfBbUZ6dJ8mBPcMX4oWiBRXJ35iUNgTbKLUUpKdc","8oRYMktcsmdbckKHob3UthYniuEpg9Zb8P3eQfnQCZed","5FNVaLqsXC9Tt1qkryoq35eAru1FDnVnMUZpGBHRhRsB","61kKMJcUeoHKqfgvuLM7PmoYTq4MT6YwFbSMsy3rwQQT","hCeZTT2NpKtwctQ2BBfQgQqKEvgHqusxeyZfSWYoJZh","E6yqAamU6bzoKaEscPiz7YKe7eaedPajfPJ6hFRQoVUu","D2zoM3FHojAMsMNL7ZGYin9wSeDhJuSdzyApcpnGTED","8stifPYgXnxCfhaNjHCrnuS2ytDvcPKMcswncnsjj9va","8BMwAcqDCgG8CaE4uAuWyoxJHWFJYg96yWc6TY1THPwR","5HGQrnKgYmzGnoF6znnJMxTmQbnd783wMLie4G1m9Fn2","jFAsdQVpHYBmcB8GDyfAixwWCqxKxViVEMDJek1HLMZ","2oaDj5BGcgSmGpBFV4Q9bRNtyYiGxYbYzjnh4bTML1Cu","2A36wxZY8BKSEHVvEsPjVa79hnPLuiLF7E64Sg6BSBk2","5fyzxa8eMACPDqah34YLnKfopq8dUtuVG7kZW4U29vt7","87wcQT9w53px3SUDa2LLTthuTLicvxNSrZzsKsmeW1hX","Dvi926R6D8Un4M4Ujx36yFmvXHB6wYvAc3bkX75axXFG","2mVcopZeBxywg3vyJofgFeN9A1Mc3a5pbk58Kk8q64zJ","qm1EDcrr4Psd1V8m7EDVSMefUnYqEgAZqtHyRBA2kPc","9ESjc5yC6Sx7QRuwvtFZnvtPzTtBes6ocuoPTDP6p4sc","3Znmdmac1ymwyV9tXPACLGSSLDF5hn2NRwEy81sTKZk4","6Ta6NYb8YeetBHvSHnu2KC1CQA6fWRsJAfGcxp3j7Guz","9NREq4QAx4YDyWwW6WDg5Z2cshD6FzmMekE7qAtmWnb1","AMQFfo1EUDnnKpxM2ZWRkj9fg4D3CcsgpD21HxQfPV3J","2JdNE5f3ienCWbt7Zi5Dsc1kYf1u75rckbtmHDUKyAsD","EAyThb6H4paoGq7idPosQw55VFRtoDW3ce5RWpm31bWx","2QKJVbaf8wiW6pUYKCUSAxufTtF6WQpYVoBrEiitKz1J","8iturbPrAbH4GRH3LiQeb9FvVyFCXBXanbsZHMfgvPHP","1MUe6h4Wov2JR66W3gR5YiuceYpfxFkSW2NMmUEhmqS","4HJiqbqzDNPS1Qq8rNdN3NkbMYaNk8nsnSkVBaxuaFe3","8jg1fy3zj1ddp5Byuc4gv9bfV1osNTNE1fCSegiaMhZJ","35o3AzLVoWe57RznUWNX4DzRawp5NnpnfzfVVPJ6wHdp","4u4qbAaT7qj4tBS6FXRdU8LRkgJVQopkNpNka2WoAN2K","A3ni4iFvTE2jtPf5FwB8JWLybGVMSUnzcBd6bmPgZx7X","JjTuxyzMYsTK4jNQATznT3qYou4po5yW7NXNBPowTxk","aiJ9W1gqpXAUyT4xfaknyhNRtujgzrmjjCbmyqTQh82","9s1NMQuXgcViYaLBwNEHA9wPtnfRx9hYiisKeYjZtRiv","oF2dw2gZ9mt8QszbusoNUq4JXBJTC4S3C6oReAXANPA","4YGDtNjDcBsmKeLAYB2jAQmcPBiFMAV3TcjNarkikH6E","2Bx4VvR86XAByMMU3KdaRpa8RunNVcAALGXks2MsQ1SN","aRswDuJQPmgsjUQjKVsEYycE5MKYitBKVJxEPceLWdB","7hNn58KR3s5DyLJQ8up99rHeoaZLL9mfP1UPm4Ls4v4d","3HSnsQa8349LhMFVQcGWPrzwMCSwgu8xGGR9gNb2HYoS","2Thoo884GZxF8a5Dn62kkYUiWsMNgDQftR5WDFQsrLpM","3aff5e7N4XK7TNRgmvSfprMPN5iP8M3x4S7TGZwhYXKo","5DGodYr26iGcV6PsWDngjauqQmj3rGBFdU3rUSWsGWjh","vHTrX9ymYDpLgWYgyuprvpducimWp8ZytjHUoLEaJqH").map(s => PublicKey25519Proposition(Base58.decode(s).get))

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
    assert(Base58.encode(genesisTxs.head.id) == "Ajvr5ocemQdcYuR1qXfKMmhSyEKZ4n4N3G9r47CJuuxx")

    val genesisBox = PublicKey25519NoncedBox(genesisAccountPriv.publicImage, 0, GenesisBalance)
    val attachment = "genesis attachment".getBytes
    val posGenesis = PosBlock.create(powGenesis.id, 0, genesisTxs, genesisBox, attachment, genesisAccountPriv)

    var history = HybridHistory.readOrGenerate(settings)
    history = history.append(powGenesis).get._1
    history = history.append(posGenesis).get._1

    val gs = HBoxStoredState.genesisState(settings, Seq(posGenesis, powGenesis))
    val gw = HWallet.genesisWallet(settings, Seq(posGenesis, powGenesis))

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