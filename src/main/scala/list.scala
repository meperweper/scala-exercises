
object list {

  implicit class ColectionOps[A](v: Node[A]) {
    def size: Int = {
      def step(v: Node[A]): Int = v match {
        case Nil() => 0
        case ValueNode(_, tail) => 1 + tail.size()
      }
      step(v)
    }

    def at(ind: Int): Node[A] = {
      def step(v: Node[A], ind: Int): Node[A] = {
        if (ind == 0) v
        else
          v match {
            case ValueNode(_, tail) => tail.at(ind - 1)
            case e => e
          }
      }
      step(v, ind)
    }

    def map[B](func: A => B): Node[B] = {
      def step(v: Node[A], func: A => B): Node[B] = v match {
        case ValueNode(el, tail) => ValueNode(func(el), tail.map(func))
        case e => Nil()
      }
      step(v, func)
    }

    def forEach(func: A => Unit): Node[A] = {
      def step(v: Node[A], func: A => Unit): Node[A] = v match {
        case Nil() => Nil()
        case ValueNode(el, tail) =>
          func(el)
          tail.forEach(func)
          v
      }
      step(v, func)
    }

    def reverse: Node[A] = {
      def step(v: Node[A], tail: Node[A]): Node[A] = v match {
        case ValueNode(el, inTail) => step(inTail, ValueNode(el, tail))
        case Nil() => tail
      }
      v match {
        case Nil() => Nil()
        case ValueNode(el, tail) => step(tail, ValueNode(el, Nil()))
      }
    }

    def append(el: A): Node[A] = {
      def step(v: Node[A], el: A): Node[A] = v match {
        case Nil() => ValueNode(el, Nil())
        case ValueNode(elem, tail) => ValueNode(elem, step(tail, el))
      }
      step(v, el)
    }

    def insert(ind: Int, el: A): Node[A] = {
      def step(v: Node[A], ind: Int): Node[A] = v match {
        case Nil() =>
          if (ind == 0) ValueNode(el, Nil())
          else Nil()
        case node@ValueNode(a, b) =>
          if (ind == 0) ValueNode(el, node)
          else ValueNode(a, step(b, ind - 1))
      }
      step(v, ind)
    }

    def delete(ind: Int): Node[A] = {
      def step(v: Node[A], ind: Int): Node[A] = v match {
        case n: Nil => n
        case ValueNode(a, b) =>
          if (ind == 0) b
          else ValueNode(a, step(b, ind - 1))
      }
      step(v, ind)
    }

    def getOrElse(num: A): A = {
      def step(v: Node[A], num: A): A = v match {
        case Nil() => num
        case ValueNode(elem, tail) => elem
      }
      step(v, num)
    }

    def isNil: Boolean = {
      def step(v: Node[A]): Boolean = v match {
        case Nil() => true
        case ValueNode(_, _) => false
      }
      step(v)
    }

    def reduce[B](func: (A, B) => B, acum: B): B = {
      def step(list: Node[A], acum: B): B = list match {
        case Nil() => acum
        case ValueNode(elem, tail) => step(tail, func(elem, acum))
      }
      step(v, acum)
    }

  }

}
