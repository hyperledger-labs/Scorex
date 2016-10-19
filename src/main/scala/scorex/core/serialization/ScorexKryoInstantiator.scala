package scorex.core.serialization

import java.io.Serializable

import com.twitter.chill.{AllScalaRegistrar, KryoBase, KryoPool, ScalaKryoInstantiator}

class ScorexKryoInstantiator extends ScalaKryoInstantiator {
  override def newKryo(): KryoBase = {
    val kryo = super.newKryo()
    val allScalaRegistrar = new AllScalaRegistrar()
    val registrar = new ScorexRegistrar()
    allScalaRegistrar(kryo)
    registrar(kryo)

    kryo
  }
}

object ScorexKryoInstantiator extends Serializable {

  private val mutex = new AnyRef with Serializable
  @transient private var pool: KryoPool = _

  def defaultPool: KryoPool = mutex.synchronized {
    if (null == pool) {
      pool = KryoPool.withByteArrayOutputStream(guessThreads, new ScorexKryoInstantiator)
    }
    pool
  }

  private def guessThreads: Int = {
    val cores = Runtime.getRuntime.availableProcessors
    val GUESS_THREADS_PER_CORE = 4
    GUESS_THREADS_PER_CORE * cores
  }
}