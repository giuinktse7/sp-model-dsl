import scala.reflect.macros.blackbox

object Macro {
  def showExpr[A](expr: A): A = macro showExpr_impl[A]

  def showExpr_impl[A: c.WeakTypeTag](c: blackbox.Context)(expr: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    expr
  }
}
