package examples.hybrid.util

import java.io.FileWriter

object FileFunctions {
  def append(filename: String, record: String): Unit = {
    val fw = new FileWriter(filename, true)
    try {
      fw.write(s"$record\n")
    }
    finally fw.close()
  }
}
