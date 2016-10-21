package io.scalac.elm.serialization

import java.nio.charset.StandardCharsets

import io.circe._
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

object JsonSerialization {
  type Codec[T] = (Encoder[T], Decoder[T])

  val byteArrayEncoder: Encoder[Array[Byte]] = Encoder[String].contramap(Base58.encode)
  val byteArrayDecoder: Decoder[Array[Byte]] = Decoder[String].map(s => Base58.decode(s).get)
}

trait JsonSerialization[T] {
  import JsonSerialization._

  def codec: Codec[T]

  def getCodec(implicit encoder: Encoder[T], decoder: Decoder[T]): Codec[T] = (encoder, decoder)

  protected implicit def baEncoder = byteArrayEncoder
  protected implicit def baDecoder = byteArrayDecoder

  private implicit def encoder = codec._1
  private implicit def decoder = codec._2

//  def bytes(m: T): Array[Byte] =
//    toJson(m).noSpaces.getBytes(StandardCharsets.UTF_8)
//
//  def parse(bytes: Array[Byte]): Try[T] =
//    parser.parse(new String(bytes, StandardCharsets.UTF_8)).toTry.flatMap(fromJson)

  def toJson(m: T): Json =
    m.asJson

  def fromJson(json: Json): Try[T] =
    json.as[T].toTry
}