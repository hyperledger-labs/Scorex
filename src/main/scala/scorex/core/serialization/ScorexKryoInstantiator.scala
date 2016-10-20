package scorex.core.serialization

import com.twitter.chill._

class ScorexKryoInstantiator(registrars: IKryoRegistrar*) extends ScalaKryoInstantiator {
  override def newKryo(): KryoBase = {
    val kryo = super.newKryo()
    val allScalaRegistrar = new AllScalaRegistrar()
    allScalaRegistrar(kryo)
    registrars.foreach(r => r.apply(kryo))

    kryo
  }
}
