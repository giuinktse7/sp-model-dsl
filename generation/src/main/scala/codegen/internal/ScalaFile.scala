package codegen.internal

import java.nio.file.{Path, Paths}

import fs2.Stream


case class ScalaFile[A: Generate] private (rootPath: Path, path: Path, name: String, contents: Seq[A]) {
  import ScalaFile.{linesToDots, stripDir, stripDots}

  val packageName: String = {
    Stream.emit(rootPath.resolve(path))
      .map(_.toString)
      .map(linesToDots).map(s => stripDir(s, rootPath)).map(stripDots)
      .toList
      .head
  }

  val fullPath: Path = rootPath.resolve(path).resolve(s"$name.scala")
}

object ScalaFile {
  private val defaultProjectDir = "generation/src/main/scala"
  private val defaultProjectPath: Path = Paths.get(defaultProjectDir)

  private def linesToDots(s: String): String = s.replaceAll("[\\\\/]", ".")
  private def stripDir(s: String, rootPath: Path): String = s.replaceAll(linesToDots(rootPath.toString), "")
  private def stripDots(s: String): String = s.replaceAll("(^\\.+|\\s+$ )|[\\.]+$", "")
  def apply[A: Generate](rootPath: Path, relativePath: Path, name: String, gs: Seq[A]): ScalaFile[A] = new ScalaFile[A](rootPath, relativePath, name, gs.toList)
  def apply[A: Generate](relativePath: Path, name: String, gs: Seq[A]): ScalaFile[A] = apply(defaultProjectPath, relativePath, name, gs)
  def apply[A <: HasType: Generate](path: Path, g: A): ScalaFile[A] = apply(path, g.`type`, Seq(g))


  def inProject[A: Generate](path: Path, name: String, contents: A*): ScalaFile[A] = new ScalaFile[A](defaultProjectPath, path, name, contents)
}