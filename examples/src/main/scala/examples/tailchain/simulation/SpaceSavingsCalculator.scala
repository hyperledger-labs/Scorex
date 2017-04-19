package examples.tailchain.simulation

import java.io.File

object SpaceSavingsCalculator extends App {

  val file = new File("results.csv")

  scala.io.Source.fromFile(file)

  file.close()

}
