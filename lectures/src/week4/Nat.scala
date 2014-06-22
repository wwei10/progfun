package week4

/* Peano numbers */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
  override def toString = "Nat"
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("Zero.predecessor")
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (that.isZero) Zero else throw new Error("Zero.-")
  override def toString = "Zero"
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def + (that: Nat): Nat = new Succ(n + that)
  def - (that: Nat): Nat =
    if (that.isZero) this else n - that.predecessor
  override def toString = "Succ(" + n + ")"
}

object TestNat {
  def main(args: Array[String]) {
    var i = 0
    println("0: " + Zero)
    var nat: Nat = Zero
    for (i <- 1 to 10) {
      nat = nat.successor
      println(i + ": " + nat)
    }
    for (i <- 9 to 0 by -1) {
      nat = nat.predecessor
      println(i + ": " + nat)
    }
    val one = new Succ(Zero)
    val five = one + one + one + one + one
    val nine = five + one + one + one + one
    println("1: " + one)
    println("5: " + five)
    println("9: " + nine)
    println("5 - 1: " + (five - one))
    println("5 - 1 - 1 - 1: " + (five - one - one - one))
    println("9 - 5: " + (nine - five))
  }
}
