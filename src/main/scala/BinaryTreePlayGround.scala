/**
 * Created by yaroslav on 24.02.2016.
 */
object BinaryTreePlayGround {
  def main(args: Array[String]) = {
    trait Node[+A] {
      def size: Int
    }

    case class Nil() extends Node[Nothing] {
      override def toString = ""

      def size = 0
    }

    case class Branch[A](head: A, left: Node[A], right: Node[A]) extends Node[A] {
      override def toString = s"$head ${left.toString} ${right.toString} "

      // def +(v: A) = ValueNode(v, this)

      override def size: Int = 1 + left.size + right.size
    }
    def >(a: Int, b: Int): Boolean = a > b
    def numberIt[A, B](v: Node[A], func: A => B, ind: Int = 0): Node[(B, Int)] = v match {
      case Branch(el, l, r) => Branch((func(el), ind), numberIt(l, func, ind + 1), numberIt(r, func, ind + 1))
      case e => Nil()
    }

    trait Ordered[T]{
      def compare(t1:T,t2:T):Boolean
    }
    implicit val intOrder = new Ordered[Int] {
      override def compare(t1: Int, t2: Int): Boolean =  t1 > t2
    }

    implicit class NodeWithsize[A](v: Node[A]) {
      def implicitAppend(el: A): Node[A] = {
        def step(v: Node[A], el: A): Node[A] = v match {
          case Nil() => Branch(el, Nil(), Nil())
          case Branch(elem, l, r) => Branch(elem, step(l, el), Nil())
        }
        step(v, el)
      }

      def +(el: A)(implicit comparator: Ordered[A]): Node[A] = {
        def step(v: Node[A], el: A): Node[A] = v match {
          case Nil() => Branch(el, Nil(), Nil())
          case Branch(elem, l, r) =>
            if (el == elem) {
              if (l.size >= r.size) Branch(elem, l, step(r, el))
              else step(l, el)
            } else if (comparator.compare(elem, el)) Branch(elem, l, step(r, el))
            else Branch(elem, step(l, el), r)
        }
        step(v, el)
      }

      def printIt(): Unit = {
        def step(v: Node[A]): Unit = v match {
          case Nil() => " nil "
          case Branch(elem, l, r) =>
            print("  " + elem)
            step(l)

            step(r)


        }
        step(v)
      }
    }
    val myTree = Branch(5, Branch(2, Nil(), Nil()), Branch(3, Branch(7, Nil(), Branch(4, Nil(), Nil())), Nil()))
    val newTree = Branch(8, Nil(), Nil()) + 3 + 9 + 9 + 11 + 2 + 33 + 4 + 5 + 5
    val kk = {
//      implicit val
    }
    println(myTree.size)
    //    newTree.printIt()
    //    println(numberIt(newTree, identity[Int]))
    val zz = newTree + 10
    println(zz.size)
  }
}
