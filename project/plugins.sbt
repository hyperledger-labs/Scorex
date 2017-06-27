// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.13.0")

addSbtPlugin("com.github.tkawachi" % "sbt-lock" % "0.3.0")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")

//addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.3")

libraryDependencies += "com.typesafe" % "config" % "1.3.0"

  //addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.0")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")
//sbt dependencyTree


addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.0")
 
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0-M1")
