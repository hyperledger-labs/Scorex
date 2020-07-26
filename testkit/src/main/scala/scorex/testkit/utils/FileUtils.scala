package scorex.testkit.utils

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import org.scalacheck.Gen

trait FileUtils {

  protected val randomPrefixLength = 10

  val basePath: Path = java.nio.file.Files.createTempDirectory(s"scorex-${System.nanoTime()}")

  sys.addShutdownHook {
    remove(basePath)
  }

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


  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def deleteRecursive(dir: java.io.File): Unit = {
    for (file <- dir.listFiles) {
      if (!file.getName.startsWith(".")) {
        if (file.isDirectory) deleteRecursive(file)
        file.delete()
      }
    }
  }

  /**
    * Recursively remove all files and directories in `root`
    */
  def remove(root: Path): Unit = {
    deleteRecursive(root.toFile)
  }

  private def createTempDirForPrefix(prefix: String): java.io.File = {
    val file = java.nio.file.Files.createTempDirectory(basePath, prefix).toFile
    file.deleteOnExit()
    file
  }

}
