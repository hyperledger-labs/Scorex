import scala.util.Try

name := "scorex-core"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.12",
  resolvers += Resolver.sonatypeRepo("public"),
  resolvers += "Maven Central Server" at "https://repo1.maven.org/maven2",
  resolvers += "Typesafe Server" at "https://repo.typesafe.com/typesafe/releases",
  wartremoverErrors ++= Seq(
    Wart.Recursion,
    Wart.TraversableOps,
    Wart.Null,
    Wart.Product,
    Wart.PublicInference,
    Wart.FinalVal,
    Wart.IsInstanceOf,
    Wart.JavaConversions,
    Wart.JavaSerializable,
    Wart.Serializable,
    Wart.OptionPartial),
  organization := "org.scorexfoundation",
  licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode")),
  homepage := Some(url("https://github.com/ScorexFoundation/Scorex")),
  pomExtra :=
      <developers>
        <developer>
          <id>kushti</id>
          <name>Alexander Chepurnoy</name>
          <url>http://chepurnoy.org/</url>
        </developer>
        <developer>
          <id>catena2w</id>
          <name>catena</name>
          <url>https://github.com/catena2w</url>
        </developer>
      </developers>,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  publishTo := sonatypePublishToBundle.value,
  fork := true // otherwise, "java.net.SocketException: maximum number of DatagramSockets reached"
)

val circeVersion = "0.13.0"
val akkaVersion = "2.6.10"
val akkaHttpVersion = "10.2.1"

val networkDependencies = Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-parsing" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-protobuf" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "org.bitlet" % "weupnp" % "0.1.4",
  "commons-net" % "commons-net" % "3.6"
)

val apiDependencies = Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "de.heikoseeberger" %% "akka-http-circe" % "1.20.0"
)

val loggingDependencies = Seq(
  "ch.qos.logback" % "logback-classic" % "1.3.0-alpha4"
)

val scorexUtil = "org.scorexfoundation" %% "scorex-util" % "0.1.8"

val testingDependencies = Seq(
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % "test",
  "org.scalactic" %% "scalactic" % "3.0.3" % "test",
  "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.+",
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,
  scorexUtil, (scorexUtil % Test).classifier("tests")
)

libraryDependencies ++= Seq(
  "com.iheart" %% "ficus" % "1.4.2",
  "org.scorexfoundation" %% "scrypto" % "2.1.10",
  scorexUtil
) ++ networkDependencies ++ apiDependencies ++ loggingDependencies ++ testingDependencies


scalacOptions ++= Seq("-Xfatal-warnings", "-feature", "-deprecation")

javaOptions ++= Seq(
  "-server"
)

testOptions in Test += Tests.Argument("-oD", "-u", "target/test-reports")

pomIncludeRepository := { _ => false }

val credentialFile = Path.userHome / ".ivy2" / ".credentials"
credentials ++= (for {
  file <- if (credentialFile.exists) Some(credentialFile) else None
} yield Credentials(file)).toSeq

lazy val testkit = Project(id = "testkit", base = file(s"testkit"))
  .dependsOn(basics)
  .settings(commonSettings: _*)

lazy val examples = Project(id = "examples", base = file(s"examples"))
  .dependsOn(basics, testkit)
  .settings(commonSettings: _*)

lazy val basics = Project(id = "scorex", base = file("."))
  .settings(commonSettings: _*)

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq


// PGP key for signing a release build published to sonatype
// signing is done by sbt-pgp plugin
// how to generate a key - https://central.sonatype.org/pages/working-with-pgp-signatures.html
// how to export a key and use it with Travis - https://docs.scala-lang.org/overviews/contributors/index.html#export-your-pgp-key-pair
pgpPublicRing := file("ci/pubring.asc")
pgpSecretRing := file("ci/secring.asc")
pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toArray)
usePgpKeyHex("D78982639AD538EF361DEC6BF264D529385A0333")

//FindBugs settings

findbugsReportType := Some(FindbugsReport.PlainHtml)

// prefix version with "-SNAPSHOT" for builds without a git tag
dynverSonatypeSnapshots in ThisBuild := true
// use "-" instead of default "+"
dynverSeparator in ThisBuild := "-"
