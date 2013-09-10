import sbt._
import Keys._
import com.typesafe.sbt.SbtScalariform._

object Properties {
  lazy val appVer         = "0.1-SNAPSHOT"
  lazy val scalaVer       = "2.10.2"
  lazy val scalaTestVer   = "2.0.M6"
 }

object BuildSettings {
  import Properties._
  lazy val buildSettings = Defaults.defaultSettings ++ Seq (
    organization        := "com.promindis",
    version             := appVer,
    scalaVersion        := scalaVer,
    scalacOptions       := Seq("-unchecked", "-deprecation"),
    ivyValidate         := false

  )
}

object Resolvers {
  lazy val typesafeReleases = "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
  lazy val scalaToolsRepo = "sonatype-oss-public" at "https://oss.sonatype.org/content/groups/public/"
}


object TestDependencies {
  import Properties._
  lazy val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVer % "test" withSources()
}

object ApplicationBuild extends Build {
  import Resolvers._
  import TestDependencies._
  import BuildSettings._

  lazy val fpInScala = Project(
    "lp-in-scala",
    file("."),
    settings = buildSettings ++ Seq(resolvers += typesafeReleases) ++  
              Seq (libraryDependencies ++= Seq(scalaTest))
  ) .settings(scalacOptions ++= Seq("-feature", "-target:jvm-1.7"))
    .settings(defaultScalariformSettings: _*)
    .settings(ScalariformKeys.preferences := CodeStyle.formattingPreferences)
}
