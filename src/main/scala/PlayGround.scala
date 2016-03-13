
object PlayGround {

  def size[A](list: Node[A]): Int = {
    list match {
      case Nil() => 0
      case ValueNode(_, tail) => 1 + size(tail)
    }
  }

  def at[A](list: Node[A], ind: Int): Node[A] = {
    if (ind == 0) list
    else
      list match {
        case ValueNode(_, tail) => at(tail, ind - 1)
        case e => e
      }
  }

  def map[A, B](func: A => B, list: Node[A]): Node[B] = {
    list match {
      case ValueNode(el, tail) => ValueNode(func(el), map(func, tail))
      case e => Nil()
    }
  }

  def forEach[A](func: A => Unit, list: Node[A]): Node[A] = {
    list match {
      case Nil() => Nil()
      case ValueNode(el, tail) =>
        func(el)
        forEach(func, tail)
        list
    }
  }

  def reverse[A](list: Node[A]): Node[A] = {
    list match {
      case ValueNode(el, tail) => append(reverse(tail), el)
      case Nil() => Nil()
    }
  }

  def append[A](list: Node[A], el: A): Node[A] = {
    list match {
      case Nil() => ValueNode(el, Nil())
      case ValueNode(elem, tail) => ValueNode(elem, append(tail, el))
    }
  }

  def reduce[A, B](list: Node[A], func: (A, B) => B, acum: B): B = list match {
    case Nil() => acum
    case ValueNode(elem, tail) => reduce(tail, func, func(elem, acum))
  }
}
