package codegen

import codegen.model.Types.ID

object Utils {
  def generateName(prefix: String, tag: String, randomChars: Int = 0): String = {
    val randomGen = ID.validIdentifier(length = randomChars)
    if (randomChars == 0) s"$randomGen${prefix}_$tag"
    else s"${randomGen}_${prefix}_$tag"
  }

  /**
    * Used to specify the kind of an F[_].
    */
  implicit class BindGenericParam[F[_]](f: F[_]) {
    def withKind[A]: F[A] = f.asInstanceOf[F[A]]
  }
}
