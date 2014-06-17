abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
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
  override def toString = elem + " {" + left + ", " + right + "} "
}

object IntSets {
  def main(args: Array[String]) {
    val t1 = new NonEmpty(1, Empty, Empty)
    val t2 = t1 incl 2
    println(t1)
    println(t2)
  }
}
