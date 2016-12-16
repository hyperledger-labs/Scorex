scalaVersion := "2.12.0"

name := "scorex-core"
organization := "org.scorexfoundation"
version := "2.0.0-M3"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

val circeVersion = "0.+"

val networkDependencies = Seq(
  "com.typesafe.akka" %% "akka-actor" % "+",
  "org.bitlet" % "weupnp" % "0.1.+",
  "commons-net" % "commons-net" % "3.+"
)

val apiDependencies = Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion
)

val loggingDependencies = Seq(
  "ch.qos.logback" % "logback-classic" % "1.+",
  "ch.qos.logback" % "logback-core" % "1.+"
)

val testingDependencies = Seq(
  "com.typesafe.akka" %% "akka-testkit" % "2.+" % "test",
  "org.scalatest" %% "scalatest" % "3.+" % "test",
  "org.scalactic" %% "scalactic" % "3.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test"
)

libraryDependencies ++= Seq(
  "org.mapdb" % "mapdb" % "3.+",
  "com.chuusai" %% "shapeless" % "2.+",
  "org.consensusresearch" %% "scrypto" % "1.2.0-RC3"
) ++ networkDependencies ++ apiDependencies ++ loggingDependencies ++ testingDependencies




libraryDependencies += "org.atnos" %% "eff" % "2.2.0"



scalacOptions ++= Seq("-feature", "-deprecation")

javaOptions ++= Seq(
  "-server"
)

testOptions in Test += Tests.Argument("-oD", "-u", "target/test-reports")

//publishing settings

publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

fork := true

pomIncludeRepository := { _ => false }

licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))

homepage := Some(url("https://github.com/ScorexFoundation/Scorex"))

pomExtra := (
  <scm>
    <url>git@github.com:ScorexFoundation/Scorex.git</url>
    <connection>scm:git:git@github.com:ScorexFoundation/Scorex.git</connection>
  </scm>
    <developers>
      <developer>
        <id>kushti</id>
        <name>Alexander Chepurnoy</name>
        <url>http://chepurnoy.org/</url>
      </developer>
    </developers>)

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
