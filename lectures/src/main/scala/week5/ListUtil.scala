package week5

object InsertionSort {

  def iSort(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case x :: xs => insert(x, iSort(xs))
  }
  
  def insert(x: Int, list: List[Int]): List[Int] = list match {
    case Nil => List(x)
    case y :: ys => if (x >= y) y :: insert(x, ys) else x :: y :: ys
  }
  
  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list.")
    case List(x) => x
    case y :: ys => last(ys)
  }
  
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list.")
    case List(x) => Nil
    case y :: ys => y :: init(ys)
  }
  
  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }
  
  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => List()
    case y :: ys => reverse(ys) ++ List(y)
  }
  
  def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ++ (xs drop (n + 1))
  
  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => List()
    case (y: List[Any]) :: ys => y ++ flatten(ys)
    case (y: Any) :: ys => y :: flatten(ys)
  }
  
  def main(args: Array[String]): Unit = {
    println("Insert: ")
    val list = List(1,2,3,4,6,7,8,9)
    println(insert(5, list))
    
    println("Insertion sort.")
    val xs = List(4,1,2,3,5)
    println(iSort(xs))
    
    val ys = List(1,2,3,4,5)
    println("Last: " + last(ys))
    println("init: " + init(ys))
    
    val zs = List(6,7,8,9,0)
    println("concat: " + concat(ys, zs))
    println("reverse: " + reverse(zs))
    println("removeAt 0: " + removeAt(0, zs))
    println("removeAt 1: " + removeAt(1, zs))
    
    val testFlatten = List(List(1,2,3), List(4,5,6), List(7,8,9))
    println("flatten: " + flatten(testFlatten))
  }

}