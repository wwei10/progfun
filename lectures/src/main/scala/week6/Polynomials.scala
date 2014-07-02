package week6

object Polynomials {
  
  class Poly(terms0: Map[Int, Double]) {
    val terms = terms0 withDefaultValue(0.0)
    def + (that: Poly): Poly =
      new Poly((that.terms foldLeft terms)(addTerm))
    def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
      val (exp, coeff) = term
      terms + (exp -> (terms(exp) + coeff))
    }
    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  def main(args: Array[String]): Unit = {
    val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
    val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
    println(p1)
    println(p2)
    println(p1 + p2)
  }

}