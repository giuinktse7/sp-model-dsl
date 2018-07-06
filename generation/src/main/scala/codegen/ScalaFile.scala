package codegen

import java.nio.file.{Path, Paths}

import fs2.Stream


case class ScalaFile[A: Generate] private (path: Path, name: String, contents: Seq[A], rootPath: Option[Path] = None) {
  import ScalaFile.{linesToDots, stripDir, stripDots}

  val packageName: String = Stream.emit(rootPath.fold(path)(_.resolve(path)))
    .map(_.toString)
    .map(linesToDots).map(stripDir).map(stripDots)
    .toList
    .head
}

object ScalaFile {
  private val defaultProjectDir = "src/main/scala"
  private val defaultProjectPath: Path = Paths.get(defaultProjectDir)

  private def linesToDots(s: String): String = s.replaceAll("[\\\\/]", ".")
  private def stripDir(s: String): String = s.replaceAll(linesToDots(defaultProjectDir), "")
  private def stripDots(s: String): String = s.replaceAll("(^\\.+|\\s+$ )|[\\.]+$", "")
  def apply[A: Generate](path: Path, name: String, first: A, rest: A*): ScalaFile[A] = new ScalaFile[A](path, name, (first +: rest).toList)
  def apply[A <: HasType: Generate](path: Path, g: A): ScalaFile[A] = apply(path, g.`type`, g)


  def inProject[A: Generate](path: Path, name: String, contents: A*): ScalaFile[A] = new ScalaFile[A](path, name, contents, rootPath = Some(defaultProjectPath))
}