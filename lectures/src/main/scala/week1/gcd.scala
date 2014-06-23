import scala.annotation.tailrec

object gcd {
  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def main(args: Array[String]) {
    println(gcd(2, 4))
    println(gcd(12, 18))
  }
}
