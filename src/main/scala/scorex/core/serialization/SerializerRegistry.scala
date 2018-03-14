package scorex.core.serialization

import io.circe.{Encoder, Json}

import scala.reflect.{ClassTag, classTag}


object SerializerRegistry {

  case class SerializerRecord[T: ClassTag](enc: Encoder[T]) {
    val ct: ClassTag[T] = classTag[T]
  }

  def apply(records: Seq[SerializerRecord[_]]): SerializerRegistry = new SerializerRegistry(records)
}

sealed class SerializerRegistry(ss: Seq[SerializerRegistry.SerializerRecord[_]]) {

  private val evTypeAndEncoder = ss.map { sr =>
    (sr.ct.runtimeClass, sr.enc)}.toMap[Class[_],  Encoder[_]]


  def toJson[C](key: Class[_], c: C): Either[Throwable, Json] = {
    evTypeAndEncoder.get(key) match {
      case Some(e) => Right(e.asInstanceOf[Encoder[C]].apply(c))
      case None => Left(new RuntimeException(s"Circe encoder is not registered for $c"))
    }
  }
}

