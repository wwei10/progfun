package week4

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  override def toString: String
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  override def toString: String = head.toString + " " + tail.toString
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def toString: String = "."
}

object List {
  // List(1 2) = List.apply(1, 2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply[T](x1: T): List[T] = new Cons(x1, new Nil)
  def apply[T]() = new Nil
}

object TestList {
  def main(args: Array[String]) {
    println(List())
    println(List(1))
    println(List(1, 2))
  }
}
