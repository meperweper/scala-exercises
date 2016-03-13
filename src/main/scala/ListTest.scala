
object ListTest {
  def main(args: Array[String]) = {

    def mult2(i: Int) = i * 2

    implicit def nodefy[A](v: A) = new {
      def toNode = ValueNode(v, Nil())
    }
    import list._

    val myList = ValueNode(3, ValueNode(5, ValueNode(7, ValueNode(1, ValueNode(2, Nil())))))
    val myList3 = "A".toNode + "b" + "c" + "d" + "e" + "f" + "g"
    println(myList.at(3))
    println(myList.map(mult2))
    println(myList.reverse)
    println(myList.size)
    println(myList.getOrElse(5))
    println(myList.isNil)
    println(myList.forEach(println))
    println(myList3.reduce((r: String, l: String) => {
      l + r
    }, ""))
    println(myList.size)
    println(myList.insert(2, 4444))
    println(myList.insert(0, 4444))
    println(myList.insert(4, 4444))
    println(myList.delete(0))
    println(myList.delete(2))
    println(myList.delete(4))

    println(myList.reverse)


  }
}
