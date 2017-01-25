name := "scorex-testkit"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.+",
  "org.scalactic" %% "scalactic" % "2.+",
  "org.scalacheck" %% "scalacheck" % "1.12.+"
)
