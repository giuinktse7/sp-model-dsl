package codegen.definition

import java.nio.file.{Path, Paths}

import cats.effect.IO
import codegen.ScalaFile
import utest._
import codegen.Generate._
import codegen.Generate.Implicits._
import org.scalafmt.Scalafmt
import fs2._

object ScalaFileTest extends TestSuite {
  def format(code: String): String = Scalafmt.format(code).get

  val tests = Tests {
    'emptyFileGen - {
      "without package name" - {
        val file = ScalaFile[EmptyDefinition](Paths.get(""), "Test", Empty)
        val genFileText: String = file.generated
        val referenceText = format("")

        assert(genFileText == referenceText)
      }
      "with package name" - {
        def testPackageName(path: Path, expected: String): IO[Unit] = {
          val file = ScalaFile[EmptyDefinition](path, "Test", Empty)

          val generated = file.generated
          IO { assert(generated == format(expected))}
        }

        val rootPath = Paths.get("src", "main", "scala")
        val pathsFromRoot = Stream(
          ("", ""),
          ("test/abc", "package test.abc"),
          ("some/other/very/long/path", "package some.other.very.long.path")
        )

        pathsFromRoot
          .flatMap { case (path, expected)  => Stream.eval(testPackageName(rootPath.resolve(Paths.get(path)), expected)) }
          .compile.drain.unsafeRunSync()
      }

    }
  }
}
