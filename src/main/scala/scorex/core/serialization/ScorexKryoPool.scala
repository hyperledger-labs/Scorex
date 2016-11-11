package scorex.core.serialization

import com.twitter.chill._

import scala.reflect.{ClassTag, _}
import scala.util.Try

class ScorexKryoPool(registrars: IKryoRegistrar*) {

  private val initiator = new ScorexKryoInstantiator(registrars)

  private val pool = KryoPool.withByteArrayOutputStream(guessThreads, initiator)

  def fromBytes[T](bytes: Array[Byte], c: Class[T]): Try[T] = Try(pool.fromBytes(bytes, c))

  def fromBytes[T: ClassTag](bytes: Array[Byte]): Try[T] = fromBytes(bytes, getClass[T])

  def toBytes[T](o: T): Array[Byte] = pool.toBytesWithoutClass(o)

  private def guessThreads: Int = {
    val cores = Runtime.getRuntime.availableProcessors
    val GUESS_THREADS_PER_CORE = 4
    GUESS_THREADS_PER_CORE * cores
  }

  private def getClass[T: ClassTag]: Class[T] = {
    classTag[T].runtimeClass.asInstanceOf[Class[T]]
  }

}
