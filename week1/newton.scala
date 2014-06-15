object newton {
  def sqrt(x: Double) = {

    def abs(x: Double) = if (x < 0) -x else x

    def isGoodEnough(guess: Double): Boolean =
      abs(guess * guess - x) < 0.001 * x

    def improve(guess: Double) =
      (guess + x / guess) / 2

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    sqrtIter(1.0)
  }

  def main(args: Array[String]) {
    println("sqrt(2): " + sqrt(2))
    println("sqrt(4): " + sqrt(4))
  }
}
