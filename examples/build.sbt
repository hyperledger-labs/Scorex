name := "scorex-examples"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "iodb" % "0.2.+",
  "com.typesafe.akka" %% "akka-testkit" % "2.+" % "test",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test"
)

mainClass in assembly := Some("examples.hybrid.HybridApp")

assemblyJarName in assembly := "twinsChain.jar"

test in assembly := {}
