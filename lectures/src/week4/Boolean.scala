package week4

abstract class Boolean {
  def ifThenElse[T](t: => T, e: => T): T
  def && (x: => Boolean) = ifThenElse(x, False)
  def || (x: => Boolean) = ifThenElse(True, x)
  def unary_! : Boolean = ifThenElse(False, True)
  def == (x: Boolean) = ifThenElse(x, x.unary_!)
  def != (x: Boolean) = ifThenElse(x.unary_!, x)
  def < (x: Boolean) = ifThenElse(False, x)
  override def toString: String
}

object True extends Boolean {
  def ifThenElse[T](t: => T, e: => T): T = t
  override def toString = "True"
}

object False extends Boolean {
  def ifThenElse[T](t: => T, e: => T): T = e
  override def toString = "False"
}

object Test {
  def main(args: Array[String]) {
    println("True: " + True)
    println("False: " + False)

    println("t and f: " + (True && False))
    println("t and t: " + (True && True))
    println("f and f: " + (False && False))
    println("f and t: " + (False && True))

    println("t or f: " + (True || False))
    println("t or t: " + (True || True))
    println("f or f: " + (False || False))
    println("f or t: " + (False || True))
  }
}
