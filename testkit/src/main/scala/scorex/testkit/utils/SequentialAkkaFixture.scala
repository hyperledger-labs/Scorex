package scorex.testkit.utils

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}

object SequentialAkkaFixture {
  val sysId = new AtomicInteger()
}

trait SequentialAkkaFixture {
  import SequentialAkkaFixture._

  class AkkaFixture extends TestKit(ActorSystem("WithIsoFix-%d".format(sysId.incrementAndGet()))) with ImplicitSender

}
