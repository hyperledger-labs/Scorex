package hybrid.history

import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import hybrid.HybridGenerators
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.core.consensus.History.{Older, HistoryComparisonResult, Equal, Younger}
import scorex.core.utils.ScorexEncoding
import scorex.core.ModifierTypeId
import scorex.util.ModifierId

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial"))
class HybridHistorySpecification extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with HybridGenerators
  with ScorexEncoding {

  var history: HybridHistory = historyGen.sample.get

  //Generate chain
  property("Block application and HybridHistory.continuationIds") {
    var ids: Seq[ModifierId] = Seq()
    forAll(posBlockGen, powBlockGen) { (posR, powR) =>
      if (history.height <= HybridHistory.DifficultyRecalcPeriod) {
        val posBlock = posR.copy(parentId = history.bestPowId)
        history = history.append(posBlock).get._1
        history.bestBlock.encodedId shouldBe posBlock.encodedId

        val powBlock = powR.copy(parentId = history.bestPowId, prevPosId = history.bestPosId, brothers = Seq(),
          brothersCount = 0)
        history = history.append(powBlock).get._1
        history.bestBlock.encodedId shouldBe powBlock.encodedId

        history.modifierById(posBlock.id).isDefined shouldBe true
        history.modifierById(powBlock.id).isDefined shouldBe true
        ids = ids ++ Seq(posBlock.id, powBlock.id)
      }
    }

    val startFrom = Seq((ModifierTypeId @@ 2.toByte, ids.head))

    history.continuationIds(startFrom, ids.length).get.map(_._2).map(encoder.encodeId) shouldEqual ids.map(encoder.encodeId)

    ids.length shouldBe HybridHistory.DifficultyRecalcPeriod

    //continuationIds with limit
    forAll(Gen.choose(0, ids.length - 1)) { startIndex: Int =>
      val startFrom = Seq((ModifierTypeId @@ 2.toByte, ids(startIndex)))
      val startList = ids.take(startIndex + 1).map(a => (ModifierTypeId @@ 2.toByte, a))
      val restIds = ids.zipWithIndex.filter { case (datum, index) => index >= startIndex }.map(_._1).map(encoder.encodeId)

      history.continuationIds(startFrom, ids.length).get.map(_._2).map(encoder.encodeId) shouldEqual restIds
      history.continuationIds(startList, ids.length).get.map(_._2).map(encoder.encodeId) shouldEqual restIds

      val limit = 5
      val continuation = history.continuationIds(startList, limit).get
      continuation.length shouldBe Math.min(limit, restIds.length)
      startList.exists(sl => sl._2 == continuation.head._2) shouldBe true
      continuation.tail.foreach { c =>
        startList.exists(sl => sl._2 == c._2) shouldBe false
      }
    }
  }

  property("History comparison") {
    (history.height >= HybridHistory.DifficultyRecalcPeriod) shouldBe true
    //TODO test for completed pairs
    history.pairCompleted shouldBe false
    testHistory(history)

    //complete pair
    forAll(posBlockGen) { posR =>
      if (!history.pairCompleted) {
        val posBlock = posR.copy(parentId = history.bestPowId)
        history = history.append(posBlock).get._1
        history.bestBlock.encodedId shouldBe posBlock.encodedId
      }
    }

    history.pairCompleted shouldBe true
    testHistory(history)

  }

  def compareAndCheck(history: HybridHistory, syncInfo: HybridSyncInfo, continuationSize: Int = 10): HistoryComparisonResult = {
    val extensionOpt = history.continuationIds(syncInfo.startingPoints, continuationSize)
    val comparison = history.compare(syncInfo)
    if (comparison == Younger) {
      println(extensionOpt)
      extensionOpt.nonEmpty shouldBe true
    }
    comparison
  }

  def testHistory(history: HybridHistory): Unit = {
    val equalsSyncInfo: HybridSyncInfo = history.syncInfo
    val lastIds = equalsSyncInfo.lastPowBlockIds
    lastIds.last shouldEqual history.bestPowId
    compareAndCheck(history, equalsSyncInfo) shouldBe Equal
    compareAndCheck(history, equalsSyncInfo.copy(lastPowBlockIds = lastIds.tail)) shouldBe Equal

    val youngerSyncInfo = equalsSyncInfo.copy(lastPowBlockIds = lastIds.dropRight(1))
    compareAndCheck(history, youngerSyncInfo) shouldBe Younger

    compareAndCheck(history, youngerSyncInfo.copy(lastPosBlockId = modifierIdGen.sample.get)) shouldBe Younger
    val posDiffComparison: HistoryComparisonResult = if (history.pairCompleted) Older else Younger
    compareAndCheck(history, equalsSyncInfo.copy(lastPosBlockId = modifierIdGen.sample.get)) shouldBe posDiffComparison

    val betterForkSyncInfo = equalsSyncInfo
      .copy(lastPowBlockIds = lastIds.dropRight(1).tail ++ Array(modifierIdGen.sample.get, modifierIdGen.sample.get))
      .copy(lastPosBlockId = modifierIdGen.sample.get)

    compareAndCheck(history, betterForkSyncInfo) shouldBe Older
  }
}
