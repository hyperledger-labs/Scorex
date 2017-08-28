package scorex.testkit.utils

trait FileUtils {

  protected val randomPrefixLength = 10

  def createTempFile: java.io.File = {
    val rndString = scala.util.Random.alphanumeric.take(randomPrefixLength).mkString
    val file = java.nio.file.Files.createTempDirectory(rndString).toFile
    file.deleteOnExit()
    file
  }

}
