package week4

import week3.{IntSet, Empty, NonEmpty}

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons[U](elem, this)
  override def toString: String
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  override def toString: String = head.toString + " " + tail.toString
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def toString: String = "."
}

object List {
  // List(1 2) = List.apply(1, 2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil))
  def apply[T](x1: T): List[T] = new Cons(x1, Nil)
  def apply[T]() = Nil
}

object TestList {
  def main(args: Array[String]) {
    println(List())
    println(List(1))
    println(List(1, 2))
  }
}

object TestVariance {
  def main(args: Array[String]) {
    val intSetList: List[IntSet] = Nil
    val nonEmptyList: List[NonEmpty] = Nil
    val list1 = intSetList.prepend(new NonEmpty(1, Empty, Empty))
    val list2 = list1.prepend(Empty)
    val list3 = nonEmptyList.prepend(new NonEmpty(1, Empty, Empty))
    val list4 = list3.prepend(Empty)
    println(list2)
    println(list4)
  }
}
