package scorex.testkit.utils

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}

object SysId {
  private val i = new AtomicInteger()
  def incrementAndGet(): Int = i.incrementAndGet()
}

class AkkaFixture
  extends TestKit(ActorSystem("WithIsoFix-%d".format(SysId.incrementAndGet())))
    with ImplicitSender
