package scorex.testkit

trait TestkitHelpers {

  val MinTestsOk = 100

  def check(minTestsOk:Int)(f: Int => Unit): Unit = (0 until minTestsOk) foreach (i => f(i))

  def check(f: Int => Unit): Unit = check(MinTestsOk)(f)
}
