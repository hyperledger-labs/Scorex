package examples.commons

import java.nio.file.{Files, Path, Paths, StandardOpenOption}

class FileLogger(filePath: String) {

  val path: Path = Paths.get(filePath)
  private val f = path.toFile
  f.getParentFile().mkdirs()
  f.createNewFile()

  def appendString(string: String): Unit = {
    Files.write(path, (string + "\n").getBytes(), StandardOpenOption.APPEND);
  }
}
