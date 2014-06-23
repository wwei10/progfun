import week4.{List, Cons, Nil}

object nth {
  def main(args: Array[String]) {
    def nth[T](n: Int, xs: List[T]): T =
      if (xs.isEmpty) throw new IndexOutOfBoundsException
      else if (n == 0) xs.head
      else nth(n - 1, xs.tail)

    val list = new Cons(1, new Cons(2, new Cons(3, Nil)))
    println(nth(0, list))
    println(nth(1, list))
    println(nth(2, list))
    println(nth(3, list))
    println(nth(-1, list))
  }
}
