

trait ColectionOps[A] {
  def size(): Int

  def at(ind: Int): Node[A]

  def map[B](func: A => B): Node[B]

  def forEach(func: A => Unit): Node[A]

  def reverse(): Node[A]

  def append(el: A): Node[A]

  def insert(ind: Int, el: A): Node[A]

  def delete(ind: Int): Node[A]

  def getOrElse(num: A): A

  def isNil: Boolean

  def reduce[A, B](list: Node[A], func: (A, B) => B, acum: B): B

}

