package scorex.testkit

trait TestkitHelpers {

  val MinTestsOk = 100
  def check(f: Int => Unit): Unit = (0 until 100) foreach (i => f(i))

}
