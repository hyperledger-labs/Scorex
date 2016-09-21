package examples.curvepos.transaction

import java.nio.ByteBuffer

import com.google.common.primitives.Ints
import scorex.core.NodeViewComponentCompanion
import scorex.core.block.StateChanges
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.utils.ScorexLogging

import scala.collection.concurrent.TrieMap
import scala.util.Try

class MinimalStateImpl extends ScorexLogging
with MinimalState[PublicKey25519Proposition, SimpleTransaction, SimpleBlock, MinimalStateImpl] {

  private val EmptyVersion: Int = 0
  private var v: Int = EmptyVersion

  private val storage: TrieMap[ByteBuffer, Box[PublicKey25519Proposition]] = TrieMap()
  private val accountIndex: TrieMap[String, PublicKey25519NoncedBox] = TrieMap()

  def isEmpty: Boolean = v == EmptyVersion

  override def version: VersionTag = Ints.toByteArray(v)

  override def accountBox(p: PublicKey25519Proposition): Option[PublicKey25519NoncedBox] = {
    accountIndex.get(p.address)
  }

  override def closedBox(boxId: Array[Byte]): Option[Box[PublicKey25519Proposition]] =
    storage.get(ByteBuffer.wrap(boxId))

  override def rollbackTo(version: VersionTag): Try[MinimalStateImpl] = {
    log.warn("Rollback is not implemented")
    Try(this)
  }


  override def applyChanges(change: StateChanges[PublicKey25519Proposition]): Try[MinimalStateImpl] = Try {
    change.toRemove.foreach(b => storage.remove(ByteBuffer.wrap(b.id)))
    change.toAppend.foreach { b =>
      storage.put(ByteBuffer.wrap(b.id), b)
      //TODO asInstanceOf
      accountIndex.put(b.proposition.address, b.asInstanceOf[PublicKey25519NoncedBox])
    }
    this
  }

  override def applyChanges(mod: SimpleBlock): Try[MinimalStateImpl] = Try {
    val generatorReward = mod.txs.map(_.fee).sum
    val generatorBox: PublicKey25519NoncedBox = accountBox(mod.generator) match {
      case Some(oldBox) => oldBox.copy(nonce = oldBox.nonce + 1, value = oldBox.value + generatorReward)
      case None => PublicKey25519NoncedBox(mod.generator, 1, generatorReward)
    }
    val toRemove = mod.txs.flatMap(_.changes(this).get.toRemove).toSet
    val toAppend = (generatorBox +: mod.txs.flatMap(_.changes(this).get.toAppend)).toSet
    val changes: StateChanges[PublicKey25519Proposition] = StateChanges[PublicKey25519Proposition](toRemove, toAppend)
    applyChanges(changes).get
  }

  override def companion: NodeViewComponentCompanion = ???

  override type NVCT = this.type
}
