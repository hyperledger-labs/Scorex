package scorex.testkit.utils

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.fixture

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object SequentialAkkaFixture {
  val sysId = new AtomicInteger()
}

trait SequentialAkkaFixture extends fixture.PropSpec {
  import SequentialAkkaFixture._
  type Fixture <: TestKit
  type FixtureParam = Fixture

  class AkkaFixture extends TestKit(ActorSystem("WithIsoFix-%d".format(sysId.incrementAndGet()))) with ImplicitSender

  def createAkkaFixture(): Fixture

  override def withFixture(test: OneArgTest) = {
    val sys = createAkkaFixture()
    try {
      test(sys)
    } finally {
      Await.result(sys.system.terminate(), Duration.Inf)
    }
  }
}
