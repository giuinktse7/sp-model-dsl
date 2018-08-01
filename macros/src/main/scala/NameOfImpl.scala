import scala.annotation.tailrec
import scala.reflect.macros._


/**
  * Taken from https://github.com/dwickern/scala-nameof/
  * 08/01/2018
  */
object NameOfImpl {
  def nameOf(c: blackbox.Context)(expr: c.Expr[Any]): c.Tree = {
    import c.universe._

    @tailrec def extract(tree: c.Tree): c.Name = tree match {
      case Ident(n) => n
      case Select(_, n) => n
      case Function(_, body) => extract(body)
      case Block(_, expression) => extract(expression)
      case Apply(func, _) => extract(func)
      case TypeApply(func, _) => extract(func)
      case _ => c.abort(c.enclosingPosition, s"Unsupported expression: $expr")
    }

    q"${extract(expr.tree).decodedName.toString}"
  }

  def nameOfType[T](c: blackbox.Context)(implicit tag: c.WeakTypeTag[T]): c.Expr[String] = {
    import c.universe._

    val name = showRaw(tag.tpe.typeSymbol.name)
    reify {
      c.Expr[String] { Literal(Constant(name)) }.splice
    }
  }

  def qualifiedNameOfType[T](c: blackbox.Context)(implicit tag: c.WeakTypeTag[T]): c.Expr[String] = {
    import c.universe._

    val name = showRaw(tag.tpe.typeSymbol.fullName)
    reify {
      c.Expr[String] { Literal(Constant(name)) }.splice
    }
  }
}
