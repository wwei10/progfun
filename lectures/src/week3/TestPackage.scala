import week2.Rational

object TestPackage {
  def main(args: Array[String]) {
    val x = new Rational(1, 2)
    val y = new Rational(1, 3)
    println(x + y)
  }
}
