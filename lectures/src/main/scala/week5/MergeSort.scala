package week5

import math.Ordering

object MergeSort {
  
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = {
        (xs, ys) match {
          case (List(), List()) => List()
          case (List(), ys1) => ys1
          case (xs1, List()) => xs1
          case (x :: xs1, y :: ys1) =>
            if (ord.lt(x, y)) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  def main(args: Array[String]): Unit = {
    val list = List(9,8,7,6,5,4,3,2,1,0)
    println(msort(list))
    
    val fruits = List("apple", "banana", "pineapple", "orange")
    println(msort(fruits))
    
    object PairOrdering extends Ordering[(Int, String)] {
      def compare(a: (Int, String), b: (Int, String)) = a._2 compare b._2
    }
    
    val pairs = List((1, "z"), (2, "y"), (3, "x"))
    println(msort(pairs)(PairOrdering))
  }
}