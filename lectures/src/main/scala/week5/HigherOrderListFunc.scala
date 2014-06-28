package week5

object HigherOrderListFunc {
  
  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case x :: xs1 => (x * x) :: squareList(xs1)
  }
  
  def squareListMap(xs: List[Int]): List[Int] = xs map (x => x * x)
  
  def posElems(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case x :: xs1 => if (x > 0) x :: posElems(xs1) else posElems(xs1)
  }
  
  def posElemsFilter(xs: List[Int]): List[Int] = xs filter (x => x > 0)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val (a, b) = xs span (y => y == x)
      a :: pack(b)
    }
  }
  
  def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs) map (x => (x.head, x.length))
  }
  
  def main(args: Array[String]): Unit = {
    val xs = List(1,2,3,4,5,6,7,8,9,10)
    println(squareList(xs))
    println(squareListMap(xs))
    val ys = List(-3,-2,-1,0,1,2,3)
    println(posElems(ys))
    println(posElemsFilter(ys))
    
    val nums = List(2, -4, 5, 7, 1)
    println(nums filter (x => x > 0))
    println(nums filterNot (x => x > 0))
    println(nums partition (x => x > 0))
    println(nums takeWhile (x => x > 0))
    println(nums dropWhile (x => x > 0))
    println(nums span (x => x > 0))
    
    val zs = List(1,1,2,2,2,3,4,5,5,1,2,2)
    println(pack(zs))
    
    println(encode(List("a", "a", "b", "b", "b", "a", "a", "c", "a", "c")))
  }

}