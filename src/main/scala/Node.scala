
trait Node[+A] {

}

case class Nil() extends Node[Nothing] {
  override def toString = ""

  def size = 0
}

case class ValueNode[A](head: A, tail: Node[A]) extends Node[A] {
  override def toString = s"$head ${tail.toString}"

  def +(v: A) = ValueNode(v, this)

}