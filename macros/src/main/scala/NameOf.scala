
/**
  * Taken from https://github.com/dwickern/scala-nameof/
  * 08/01/2018
  */
trait NameOf {
  import scala.language.experimental.macros

  def nameOf(expr: Any): String = macro NameOfImpl.nameOf
  def nameOf[T](expr: T => Any): String = macro NameOfImpl.nameOf
  def nameOfType[T]: String = macro NameOfImpl.nameOfType[T]
  def qualifiedNameOfType[T]: String = macro NameOfImpl.qualifiedNameOfType[T]
}
object NameOf extends NameOf