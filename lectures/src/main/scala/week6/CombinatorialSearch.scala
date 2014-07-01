package week6

object CombinatorialSearch {
  
  def isPrime(n: Int): Boolean = (2 until n) forall (x => n % x != 0)
  
  def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for (xy <- xs zip ys) yield xy._1 * xy._2).sum 

  def main(args: Array[String]): Unit = {
    val n = 7
    /* flatMap = map().flatten */
    println(((1 until n) map (i => (1 until i) map (j => (i, j)))).flatten)
    println((1 until n) flatMap (i => (1 until i) map (j => (i, j))))
    println(((1 until n) flatMap (i =>
      (1 until i) map (j => (i, j)))) filter (pair =>
        isPrime(pair._1 + pair._2)))
    println(for {
      i <- 1 until n
      j <- 1 until i
      if isPrime(i + j)
    } yield (i, j))
    println(scalarProduct(List(1, 2, 3), List(3, 2, 1)))
  }

}