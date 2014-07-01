package week6

object VectorOps {
  
  def combinations(m: Int, n: Int): List[(Int, Int)] =
    (1 to m).toList flatMap (x => (1 to n).toList map (y => (x, y)))
  
  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
    (xs zip ys).map(xy => xy._1 * xy._2).sum
  }
  
  def isPrime(n: Int): Boolean =
    (2 to Math.sqrt(n).toInt).forall(x => n % x != 0)
  
  def main(args: Array[String]) {
    val a = Vector(1, 2, 3);
    println(a)
    val b = a :+ 4
    println(b)
    val c = 0 +: a
    println(c)
    println(b zip c)
    println(a exists (x => x > 2))
    println(a forall (x => x > 0))
    println((a flatMap (x => List('.', x.toString))).foldLeft("")((a, b) => a + b))
    println(a.max)
    println(a.sum)
    println(combinations(5, 5))
    println(scalarProduct(Vector(1, 2, 3), Vector(3, 2, 1)))
    var i = 0
    for (i <- 1 to 10) {
      println(i + " is prime: " + isPrime(i))
    }
  }
}