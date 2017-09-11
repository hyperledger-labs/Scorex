package scorex.testkit.utils

import org.scalacheck.Gen

trait FileUtils {

  protected val randomPrefixLength = 10

  def createTempFile: java.io.File = {
    val dir = createTempDir
    val prefix = scala.util.Random.alphanumeric.take(randomPrefixLength).mkString
    val suffix = scala.util.Random.alphanumeric.take(randomPrefixLength).mkString
    val file = java.nio.file.Files.createTempFile(dir.toPath, prefix, suffix).toFile
    file.deleteOnExit()
    file
  }

  def createTempDir: java.io.File = {
    val rndString = scala.util.Random.alphanumeric.take(randomPrefixLength).mkString
    createTempDirForPrefix(rndString)
  }

  def tempDirGen: Gen[java.io.File] = Gen.listOfN(randomPrefixLength, Gen.alphaNumChar).map { p =>
    val prefix = p.mkString("")
    createTempDirForPrefix(prefix)
  }

  private def createTempDirForPrefix(prefix: String): java.io.File = {
    val file = java.nio.file.Files.createTempDirectory(prefix).toFile
    file.deleteOnExit()
    file
  }

}
