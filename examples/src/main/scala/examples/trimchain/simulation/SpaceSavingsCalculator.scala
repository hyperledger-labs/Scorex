package examples.trimchain.simulation

import java.io.File

import examples.commons.PublicKey25519NoncedBox

object SpaceSavingsCalculator extends App {

  val eta = 100
  val start = 1000
  val finish = 10000

  val outSize = PublicKey25519NoncedBox.BoxLength

  val file = new File("/home/pozharko/Code/papers/trimchain/results-20.csv")

  val lines = scala.io.Source.fromFile(file).getLines().toIndexedSeq

  //  println(lines.head)

  // TODO: fixme, What should we do if `lines` is empty?
  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  val data = lines.tail.take(finish).map(_.split(","))

  val blockSizes = data.map(_.apply(8)).map(_.toLong)

  val headerSizes = data.map(_.apply(5)).map(_.toLong)

  val currentUtxoSizes = data.map(_.apply(2)).map(_.toLong * outSize)

  println(s"height,full,spv,light,mining,f/s,f/l,f/m")
  (start to finish).foreach{h =>
    val fullChain = blockSizes.take(h).sum
    val lightChain = headerSizes.take(h - eta).sum + blockSizes.slice(h - eta, h).sum

    val spvchain = headerSizes.take(h).sum

    val fullSet = fullChain + currentUtxoSizes(h - 1)

    val lightSet = lightChain + currentUtxoSizes(h - 1)

    val miningSet = lightSet + currentUtxoSizes(h - eta - 1)

    println(s"$h,$fullSet,$spvchain,$lightSet,$miningSet,${fullSet/spvchain.toDouble},${fullSet / lightSet.toDouble},${fullSet / miningSet.toDouble}")
  }
}
