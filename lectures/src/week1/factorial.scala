object fatorial {

  def factorial(n: Int): Int = {
    def factorial2(n: Int, acc: Int): Int = {
      if (n == 0) acc else factorial2(n - 1, acc * n)
    }
    factorial2(n, 1)
  }

  def main(args: Array[String]) {
    var i = 0;
    for (i <- 1 to 10) {
      println(factorial(i))
    }
  }
}
