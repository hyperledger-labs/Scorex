package hybrid.history

import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import hybrid.HybridGenerators
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.consensus.History.HistoryComparisonResult
import scorex.crypto.encode.Base58
import scorex.utils.Random


class HybridHistorySpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {

  var history = generateHistory


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

    val startFrom = ids.head

    history.continuationIds(Seq((2.toByte, startFrom)), ids.length).get.map(_._2).map(Base58.encode) shouldEqual ids.map(Base58.encode)

    ids.length shouldBe HybridHistory.DifficultyRecalcPeriod

    //continuationIds with limit
    forAll(Gen.choose(0, ids.length - 1)) { startIndex: Int =>
      val startFrom = Seq((2.toByte, ids(startIndex)))
      val startList = ids.take(startIndex + 1).map(a => (2.toByte, a))
      val restIds = ids.zipWithIndex.filter { case (datum, index) => index >= startIndex }.map(_._1).map(Base58.encode)

      history.continuationIds(startFrom, ids.length).get.map(_._2).map(Base58.encode) shouldEqual restIds
      history.continuationIds(startList, ids.length).get.map(_._2).map(Base58.encode) shouldEqual restIds

      val limit = 5
      val continuation = history.continuationIds(startList, limit).get
      continuation.length shouldBe Math.min(limit, restIds.length)
      startList.exists(sl => sl._2 sameElements continuation.head._2) shouldBe true
      continuation.tail.foreach { c =>
        startList.exists(sl => sl._2 sameElements c._2) shouldBe false
      }
    }
  }

  property("History comparison") {
    assert(history.height >= HybridHistory.DifficultyRecalcPeriod)
    assert(!history.pairCompleted)

    val equalsSyncInfo: HybridSyncInfo = history.syncInfo(false)
    val lastIds  = equalsSyncInfo.lastPowBlockIds
    lastIds.last shouldEqual history.bestPowId
    compareAndCheck(history, equalsSyncInfo) shouldBe HistoryComparisonResult.Equal
    compareAndCheck(history, equalsSyncInfo.copy(lastPowBlockIds = lastIds.tail)) shouldBe HistoryComparisonResult.Equal

    val youngerSyncInfo = equalsSyncInfo.copy(lastPowBlockIds = lastIds.dropRight(1))
    compareAndCheck(history, youngerSyncInfo) shouldBe HistoryComparisonResult.Younger

    compareAndCheck(history, youngerSyncInfo.copy(lastPosBlockId = Random.randomBytes(32))) shouldBe HistoryComparisonResult.Younger
    compareAndCheck(history, equalsSyncInfo.copy(lastPosBlockId = Random.randomBytes(32))) shouldBe HistoryComparisonResult.Younger

    val betterForkSyncInfo = equalsSyncInfo
      .copy(lastPowBlockIds = lastIds.dropRight(1).tail ++ Array(Random.randomBytes(32), Random.randomBytes(32)))
      .copy(lastPosBlockId = Random.randomBytes(32))

    compareAndCheck(history, betterForkSyncInfo) shouldBe HistoryComparisonResult.Older

  }

  def compareAndCheck(history: HybridHistory, syncInfo: HybridSyncInfo, networkChunkSize: Int = 10): HistoryComparisonResult.Value = {
    val extensionOpt = history.continuationIds(syncInfo.startingPoints, networkChunkSize)
    val comparison = history.compare(syncInfo)
    if (comparison == HistoryComparisonResult.Younger) {
      println(extensionOpt)
      extensionOpt.nonEmpty shouldBe true
    }
    comparison
  }

}
