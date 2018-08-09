package types

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object Generate {
  def funImpl[T: c.WeakTypeTag](c: whitebox.Context)(tree: c.Tree): c.Tree = {
    import c.universe._

    tree match {
      case Select(a, n) => println(showRaw(a), showRaw(n))
      case x => println(showRaw(x))
    }
      q"""{
         (x: Any) => implicitly[codegen.Generate[lol]].generated(x.asInstanceOf[lol])
        }
        """
  }

  def fun[T](tree: String): Any => Any = macro funImpl[T]
}
