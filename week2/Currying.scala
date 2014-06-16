object Currying {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    mapReduce(f, (a: Int, b: Int) => a + b, 0)(a, b)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    mapReduce(f, (a: Int, b: Int) => a * b, 1)(a, b)
  }

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }

  def sumInts = sum(x => x) _
  def sumCubes = sum(x => x * x * x) _
  def sumFactorials = sum(fact) _

  def fact(n: Int): Int = product(x => x)(1, n)

  def main(args: Array[String]) {
    println(sumInts(1, 100))
    println(sumCubes(1, 5))
    println(sumFactorials(1, 5))
  }
}
