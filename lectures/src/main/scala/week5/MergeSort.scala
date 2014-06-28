package week5

object MergeSort {
  
  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = {
        (xs, ys) match {
          case (List(), List()) => List()
          case (List(), ys1) => ys1
          case (xs1, List()) => xs1
          case (x :: xs1, y :: ys1) =>
            if (x < y) x :: merge(xs1, ys)
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
  }

}