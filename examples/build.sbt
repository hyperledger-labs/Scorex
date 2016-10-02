name := "scorex-basics"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-testkit" % "2.+" % "test",
  "org.scalatest" %% "scalatest" % "2.+" % "test",
  "org.scalactic" %% "scalactic" % "2.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.+" % "test",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test"
)
