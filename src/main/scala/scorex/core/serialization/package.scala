package scorex.core

import scala.reflect._

package object serialization {
  def getClassFromClassTag[T: ClassTag]: Class[T] = {
    classTag[T].runtimeClass.asInstanceOf[Class[T]]
  }
}
