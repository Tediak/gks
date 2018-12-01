package gks

case class Module(operations: List[String]) {

  val isEmpty: Boolean = operations.isEmpty

  val nonEmpty: Boolean = operations.nonEmpty

  def +(value: String) = Module(this.operations ::: List(value))

  def ++(that: Module) = Module(this.operations ::: that.operations)

  override def toString: String = s"{ ${operations.mkString(" ")} }"
}

object Module {
  val Empty = Module(Nil)

  def apply(): Module = Module.Empty

  implicit class ModuleCollectionExtender(modules: List[Module]) {
    val show: () => Unit = () => modules foreach println

    val getAllOperations: () => List[String] = () => modules.flatMap(_.operations)
  }
}

