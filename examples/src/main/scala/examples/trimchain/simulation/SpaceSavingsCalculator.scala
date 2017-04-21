package examples.trimchain.simulation

import java.io.File

import examples.curvepos.transaction.PublicKey25519NoncedBox

/*
  set ylabel 'Megabytes'
  set xlabel 'Blocks'

  plot 'ps20.csv' using 1:5 with lines title 'TrimChain SPV Node', 'ps20.csv' using 1:3 with lines title 'Trimchain Mining Node', 'ps20.csv' using 1:4 with lines title 'TrimChain Non-mining Node', 'ps20.csv' using 1:2 with lines title 'Full Node'
 */
object SpaceSavingsCalculator extends App {

  val eta = 100
  val start = 1000
  val finish = 9500

  val outSize = PublicKey25519NoncedBox.BoxLength

  val file = new File("/home/kushti/garbage/results-20.csv")

  val lines = scala.io.Source.fromFile(file).getLines().toIndexedSeq

  //println(lines.head)
  //println(lines.tail.head)


  val data = lines.tail.take(finish).map(_.split(","))

  val blockSizes = data.map(_.apply(8)).map(_.toLong + 870000)

  val headerSizes = data.map(_.apply(5)).map(_.toLong)

  val currentUtxoSizes = data.map(_.apply(2)).map(_.toLong * outSize)

  (start to finish).foreach{h =>
    val fullChain = blockSizes.take(h).sum
    val lightChain = headerSizes.take(h - eta).sum + blockSizes.slice(h - eta, h).sum

    val spvchain = headerSizes.take(h).sum

    val fullSet = fullChain + currentUtxoSizes(h - 1)

    val lightSet = lightChain + currentUtxoSizes(h - 1)

    val miningSet = lightSet + currentUtxoSizes(h - eta - 1)

    if(h%200 == 0) {
      println(h + "," + fullSet / 1000000 + "," + miningSet / 1000000 + "," + lightSet / 1000000 + "," + spvchain  / 1000000)
    }
  //  println(s"height: $h, full: $fullSet, spv: $spvchain, light: $lightSet, mining: $miningSet, " +
  //      s"f/s: ${fullSet/spvchain.toDouble}, f/l: ${fullSet / lightSet.toDouble}, f/m: ${fullSet / miningSet.toDouble}")
  }
}
