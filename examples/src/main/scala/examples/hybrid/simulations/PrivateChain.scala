package examples.hybrid.simulations

import examples.commons.SimpleBoxTransactionMemPool
import examples.hybrid.HybridNodeViewHolder
import examples.hybrid.blocks.{PosBlock, PowBlock}
import examples.hybrid.history.HybridHistory
import examples.hybrid.mining.{HybridSettings, PosForger, PowMiner}
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.util.FileFunctions
import examples.hybrid.wallet.HBoxWallet
import scorex.core.block.Block.BlockId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.{NetworkTimeProvider, ScorexEncoding, ScorexLogging}
import scorex.core.crypto.encode.Base58
import scorex.core.crypto.signatures.PublicKey

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.Try

/**
  * Private chain attack simulation
  */
object PrivateChain extends App with ScorexLogging with ScorexEncoding {

  val proposition: PublicKey25519Proposition = PublicKey25519Proposition(PublicKey @@ scorex.utils.Random.randomBytes(32))

  def genesisState(): (HybridHistory, HBoxStoredState, HBoxWallet, SimpleBoxTransactionMemPool)  = {
    HybridNodeViewHolder.generateGenesisState(hybridSettings, new NetworkTimeProvider(settings.ntp))
  }

  def generatePow(h: HybridHistory, brother: Boolean, hashesPerSecond: Int): PowBlock = {
    val difficulty = h.powDifficulty

    val (parentId, prevPosId, brothers) = if (!h.pairCompleted) {
      //brother
      log.info(s"Starting brother mining for ${encoder.encode(h.bestPowBlock.parentId)}:${encoder.encode(h.bestPowBlock.parentId)}")
      val bs = h.bestPowBlock.brothers :+ h.bestPowBlock.header
      (h.bestPowBlock.parentId, h.bestPowBlock.parentId, bs)
    } else {
      log.info(s"Starting new block mining for ${encoder.encode(h.bestPowId)}:${encoder.encode(h.bestPosId)}")
      (h.bestPowId, h.bestPosId, Seq()) //new step
    }

    @tailrec
    def step(): PowBlock = {
      val delay = (1000 / hashesPerSecond).seconds
      PowMiner.powIteration(parentId, prevPosId, brothers, difficulty, hybridSettings.mining, proposition, delay) match {
        case Some(block) => block
        case None => step()
      }
    }
    step()
  }

  private val hybridSettings = HybridSettings.read(Some("settings.conf"))
  implicit lazy val settings = hybridSettings.scorexSettings
  lazy val miningSettings = hybridSettings.mining

  def timeSpent(adversarialStakePercent: Int, hashesPerSecond: Int): Long = {

    Path.apply(settings.dataDir).deleteRecursively()

    var (history, _, wallet, _) = genesisState()

    val boxes = wallet.boxes().map(_.box).take(adversarialStakePercent)
    val boxKeys = boxes.flatMap(b => wallet.secretByPublicImage(b.proposition).map(s => (b, s)))

    println("Boxes set size: " + boxes.size)

    // TODO: review me, could `firstId` still be null at the end of the do-while loop?
    @SuppressWarnings(Array("org.wartremover.warts.Null"))
    var firstId: BlockId = null

    do {
      if (history.isEmpty || history.pairCompleted) {
        val b = generatePow(history, brother = false, hashesPerSecond)
        if (history.isEmpty) firstId = b.id
        history = history.append(b).get._1
        Thread.sleep(15)
      } else {

        val target = hybridSettings.mining.MaxTarget / history.posDifficulty

        @tailrec
        def posStep(): PosBlock = {
          val pb = history.bestPowBlock
          PosForger.posIteration(pb, boxKeys, Seq(), Array(), target) match {
            case Some(pos) => pos
            case None =>
              val npb = generatePow(history, brother = true, hashesPerSecond)
              history = history.append(npb).get._1
              posStep()
          }
        }
        val b = posStep()
        history = history.append(b).get._1
        Thread.sleep(15)
      }
    } while (!(history.height == 10 && history.pairCompleted))

    // TODO: review me - .get
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val timestampDifference =
      history.bestPosBlock.timestamp - history.modifierById(firstId).get.asInstanceOf[PowBlock].timestamp
    timestampDifference
  }

  val experiments: Int = 2

  val honestHashesPerSecond: Int = 50

  val honestAvg = (1 to experiments).map { _ =>
    timeSpent(20, honestHashesPerSecond)
  }.sum / experiments.toDouble

  println("avg honest time = " + honestAvg)

  def experiment(adversarialStakePercent: Int, advHashesPerSecond: Int): Try[Unit] = Try {
    val advSuccess = (1 to experiments).map { _ =>
      val advTime = timeSpent(adversarialStakePercent, advHashesPerSecond)
      println(s"times: adversarial: $advTime, honest: $honestAvg")
      if (advTime < honestAvg) 1 else 0
    }.sum

    val row = s"$adversarialStakePercent, ${advHashesPerSecond / honestHashesPerSecond.toDouble}, $advSuccess"
    println(s"Got data row: $row")
    FileFunctions.append("/home/kushti/test.csv", row)
  }

  (1 to 3).foreach { asp5 =>
    val adversarialStakePercent = asp5 * 5
    (8 to 10).foreach { ahp =>
      val advHashesPerSecond = ahp * honestHashesPerSecond
      experiment(adversarialStakePercent, advHashesPerSecond)
        .getOrElse(experiment(adversarialStakePercent, advHashesPerSecond))
    }
  }
}