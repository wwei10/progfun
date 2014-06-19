abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
  def union(other: IntSet): IntSet = other
  override def toString = " E "
}

class NonEmpty(x: Int, leftTree: IntSet, rightTree: IntSet) extends IntSet {
  def elem = x
  def left = leftTree
  def right = rightTree
  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else new NonEmpty(elem, left, right incl x)
  }
  def contains(x: Int): Boolean = {
    if (x == elem) true
    else if (x < elem) left contains x
    else right contains x
  }
  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem
  override def toString = elem + " {" + left + ", " + right + "} "
}

object IntSets {
  def main(args: Array[String]) {
    val t1 = new NonEmpty(1, Empty, Empty)
    val t2 = t1 incl 2
    val t3 = t2 incl 3
    val t4 = t3 incl 4
    val t5 = t4 incl 5
    val t6 = new NonEmpty(6, Empty, Empty)
    val t7 = t6 incl 7
    val t8 = t7 incl 8
    println(t5)
    println(t8)
    println(t5 union t8)
  }
}
