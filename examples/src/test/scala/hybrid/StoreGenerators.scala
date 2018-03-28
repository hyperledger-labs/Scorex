package hybrid

import io.iohk.iodb.{LSMStore, QuickStore}
import org.scalacheck.Gen
import scorex.testkit.utils.FileUtils

trait StoreGenerators extends FileUtils {

  protected val minKeepVersions = 10
  protected val maxKeepVersions = 20

  protected lazy val keepVersionsGen = Gen.chooseNum(minKeepVersions, maxKeepVersions)

  lazy val lsmStoreGen: Gen[LSMStore] = for {
    dir <- tempDirGen
    keepVersions <- keepVersionsGen
  } yield new LSMStore(dir, keepVersions = keepVersions)

  lazy val quickStoreGen: Gen[QuickStore] = for {
    dir <- tempDirGen
    keepVersions <- keepVersionsGen
  } yield new QuickStore(dir, keepVersions = keepVersions)

}
