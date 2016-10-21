package io.scalac.elm.config

import scorex.core.app.ApplicationVersion


object AppInfo {
  //FIXME: take values from build file or config
  val name = "elm"
  val version = "1.0.0"

  val appVersion = {
    //FIXME: catch exception, inform of the proper version format
    val major :: minor :: rev :: Nil = version.split("\\.").toList.map(_.toInt)
    ApplicationVersion(major, minor, rev)
  }
}
