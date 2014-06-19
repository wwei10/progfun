import math.abs

object FixedPoint {
  val tolerance = 0.0001
  def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
    def isCloseEnough(guess: Double) =
      math.abs(f(guess) - guess) < tolerance * guess
    def iterate(guess: Double): Double =
      if (isCloseEnough(guess)) guess
      else iterate(f(guess))
    iterate(firstGuess)
  }

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def main(args: Array[String]) {
    println("x => 1 + x / 2: " + fixedPoint(x => 1 + x / 2)(1))
  }
}
