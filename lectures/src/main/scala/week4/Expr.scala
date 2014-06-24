package week4

trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

object Eval {
  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
    case Prod(e1, e2) => eval(e1) * eval(e2)
  }
  
  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => "(" + show(e1) + " + " + show(e2) + ")"
    case Prod(e1, e2) => "(" + show(e1) + " * " + show(e2) + ")"
  }
  
  def main(args: Array[String]) {
    println(show(Sum(Number(1), Sum(Number(2), Number(3)))))
    println(eval(Sum(Number(10), Number(14))))
    println(show(Prod(Sum(Number(2), Number(3)), Number(4))))
    println(eval(Prod(Sum(Number(2), Number(3)), Number(4))))
  }
}