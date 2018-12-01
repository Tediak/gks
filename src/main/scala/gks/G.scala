package gks

case class G (
             name: String,
             subnodes: Set[String]
             ) {

  def ++(set: Set[String]): G = G(name, subnodes ++ set)

  def +(subnode: String): G = G(name, subnodes + subnode)

}

object G {
  def apply(name: String): G = G(name, Set())
}