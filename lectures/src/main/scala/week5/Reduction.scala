package week5

object Reduction {
  
  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys)(_ :: _)
    
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())((a, b) => f(a) :: b)
  
  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((_, len) => 1 + len)

  def main(args: Array[String]): Unit = {
    println(concat("abc".toList, "def".toList))
    println(mapFun[Char, Char]("abc".toList, x => x.toUpper))
    println(lengthFun[Char]("abcdef".toList))
    println(lengthFun[Char]("abcdefghij".toList))
  }

}