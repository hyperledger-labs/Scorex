name := "scorex-examples"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "org.scorexfoundation" %% "iodb" % "0.3.1",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.17" % "test",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test"
)

mainClass in assembly := Some("examples.hybrid.HybridApp")
//mainClass in assembly := Some("examples.trimchain.simulation.OneMinerSimulation")

assemblyJarName in assembly := "twinsChain.jar"

parallelExecution in Test := true

testForkedParallel in Test := true

test in assembly := {}
