package names

trait WithType {
  def tpe: String
}

trait NameChangeable[T] {
  def setName(name: String): T
}
