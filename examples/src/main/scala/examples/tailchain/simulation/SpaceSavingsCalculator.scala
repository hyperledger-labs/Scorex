package examples.tailchain.simulation

import java.io.File

import examples.curvepos.transaction.PublicKey25519NoncedBox

object SpaceSavingsCalculator extends App {

  val eta = 100
  val start = 1000
  val finish = 10000

  val outSize = PublicKey25519NoncedBox.BoxLength

  val file = new File("/home/kushti/garbage/results.csv")

  val lines = scala.io.Source.fromFile(file).getLines().toIndexedSeq

  println(lines.head)

  val data = lines.tail.take(finish).map(_.split(","))

  val blockSizes = data.map(_.apply(8)).map(_.toLong)

  val headerSizes = data.map(_.apply(5)).map(_.toLong)

  val currentUtxoSizes = data.map(_.apply(2)).map(_.toLong * outSize)

  (start to finish).foreach{h =>
    val fullChain = blockSizes.take(h).sum
    val lightChain = headerSizes.take(h - eta).sum + blockSizes.slice(h - eta, h).sum

    val spvchain = headerSizes.take(h).sum

    val fullSet = fullChain + currentUtxoSizes(h - 1)

    val lightSet = lightChain + currentUtxoSizes(h - 1)

    val miningSet = lightSet + currentUtxoSizes(h - eta - 1)

    println(s"height: $h, full: $fullSet, spv: $spvchain, light: $lightSet, mining: $miningSet, " +
      s"f/s: ${fullSet/spvchain.toDouble}, f/l: ${fullSet / lightSet.toDouble}, f/m: ${fullSet / miningSet.toDouble}")
  }
}
