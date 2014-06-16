object Sum {
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }
  def sumInts(a: Int, b: Int) = sum(x => x, a, b)
  def sumCubes(a: Int, b: Int) = sum(x => x * x * x, a, b)
  def sumFactorials(a: Int, b: Int) = sum(fact, a, b)

  def fact(x: Int): Int = if (x == 0) 1 else fact(x - 1) * x

  def main(args: Array[String]) {
    println("Sum 1 - 100: " + sumInts(1, 100))
    println("Cube sum 1 - 5: " + sumCubes(1, 5))
    println("Factorial sum 1 - 5: " + sumFactorials(1, 5))
  }
}
