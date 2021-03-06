import Dependencies._



lazy val commonSettings = Seq(
  version := "0.1",
  scalaVersion := "2.12.3",
  autoCompilerPlugins := true,
  scalacOptions ++= compilerOptions,
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full),
  libraryDependencies ++= commonDependencies,
  testFrameworks := Seq(new TestFramework("utest.runner.Framework"))
)


lazy val macros = (project in file("macros"))
  .settings(commonSettings)


lazy val generation = (project in file("generation"))
  .settings(commonSettings ++ Seq(
    mainClass in (Compile, run) := Some("Main"),
    rootDependencies
  ))
  .dependsOn(macros)

lazy val root = (project in file(".")).aggregate(macros, generation)