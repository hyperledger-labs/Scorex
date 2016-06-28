name := "scorex-basics"

scalaVersion := "2.11.8"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

libraryDependencies ++= Seq (
  "com.google.guava" % "guava" % "18.+",
  "com.typesafe.play" %% "play-json" % "2.4.+",
    "com.typesafe.akka" %% "akka-actor" % "2.+",
    "org.bitlet" % "weupnp" % "0.1.+",
    "com.h2database" % "h2-mvstore" % "1.+",
    "com.typesafe.akka" %% "akka-http-experimental" % "2.+",
    "com.chuusai" %% "shapeless" % "2.+",
    "io.swagger" %% "swagger-scala-module" % "1.+",
    "io.swagger" % "swagger-core" % "1.+",
    "io.swagger" % "swagger-annotations" % "1.+",
    "io.swagger" % "swagger-models" % "1.+",
    "io.swagger" % "swagger-jaxrs" % "1.+",
    "com.github.swagger-akka-http" %% "swagger-akka-http" % "0.+",
    "com.typesafe.akka" %% "akka-testkit" % "2.+" % "test",
    "org.scalatest" %% "scalatest" % "2.+" % "test",
    "org.scalactic" %% "scalactic" % "2.+" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.+" % "test",
    "net.databinder.dispatch" %% "dispatch-core" % "+" % "test",
    "ch.qos.logback" % "logback-classic" % "1.+",
    "ch.qos.logback" % "logback-core" % "1.+",
    "org.consensusresearch" %% "scrypto" % "1.2.0-RC1",
    "commons-net" % "commons-net" % "3.+"
)
