import sbt.Keys.libraryDependencies
import sbt._

object Dependencies {
  lazy val scalaVersionNo = "2.12.3"
  lazy val monocleVersion = "1.5.0"
  lazy val paradiseVersion = "2.1.0"

  lazy val commonDependencies = Seq(
    "com.lihaoyi" %% "utest" % "0.6.3" % "test",
    "org.scala-lang" % "scala-reflect" % "2.12.3"
  )

  lazy val rootDependencies = libraryDependencies ++= Seq(
    "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion,
    "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion,
    "com.github.julien-truffaut" %%  "monocle-law"   % monocleVersion % "test",

    "co.fs2" %% "fs2-core" % "0.10.1",
    "co.fs2" %% "fs2-io" % "0.10.1",

    "com.typesafe.play" %% "play-json" % "2.6.0",
    "com.geirsson" %% "scalafmt-core" % "1.6.0-RC3",
    "org.typelevel" %% "cats-core" % "1.2.0"
  )

  lazy val compilerOptions = Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfuture",                          // Turn on future language features.
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Ypartial-unification",             // Enable partial unification in type constructor inference
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused:implicits"           // Warn if an implicit parameter is unused.
  )
}