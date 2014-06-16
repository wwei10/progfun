import FixedPoint.{fixedPoint, averageDamp}

object Sqrt {
  def sqrt(x: Double): Double = {
    fixedPoint(y => averageDamp(y => x / y)(y))(1)
  }

  def main(args: Array[String]) {
    var i = 0
    for (i <- 1 to 10)
      println(sqrt(i))
  }
}
