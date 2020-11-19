name := "scorex-testkit"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1",
  "org.scalacheck" %% "scalacheck" % "1.13.+",
  "com.typesafe.akka" %% "akka-testkit" % "2.6.10"
)

fork in Test := true

javaOptions in Test ++= Seq("-Xmx2G")

parallelExecution in Test := false