import sbt.Keys.libraryDependencies
import sbt._

object Dependencies {
  lazy val scalaVersionNo = "2.12.3"
  lazy val monocleVersion = "1.5.0"
  lazy val paradiseVersion = "2.1.0"

  lazy val commonDependencies = Seq(
    "com.lihaoyi" %% "utest" % "0.6.3" % "test"
  )

  lazy val rootDependencies = libraryDependencies ++= Seq(
      "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion,
      "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion,
      "com.github.julien-truffaut" %%  "monocle-law"   % monocleVersion % "test",

      "co.fs2" %% "fs2-core" % "0.10.1",
      "co.fs2" %% "fs2-io" % "0.10.1",

      "com.typesafe.play" %% "play-json" % "2.6.0",
      "com.geirsson" %% "scalafmt-core" % "1.6.0-RC3"
    )
}