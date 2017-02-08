name := "scorex-examples"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "iodb" % "0.2.+",
  "com.typesafe.akka" %% "akka-testkit" % "2.+" % "test",
  "org.scalatest" %% "scalatest" % "2.+" % "test",
  "org.scalactic" %% "scalactic" % "2.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.+" % "test",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test"
)

mainClass in assembly := Some("examples.hybrid.HybridApp")

assemblyJarName in assembly := "twinsChain.jar"

test in assembly := {}
