package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
  
  test("times of some word") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("combine nil or singleton") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) === leaflist)
    assert(combine(Nil) == Nil)
  }
    
  test("decode a small list of chars") {
    new TestTrees {
      assert(decode(t1, List(0)) === "a".toList)
      assert(decode(t1, List(1)) === "b".toList)
      assert(decode(t1, List(0, 1)) === "ab".toList)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, encode(t2)("abdba".toList)) === "abdba".toList)
    }
  }
  
  test("quick encode") {
    new TestTrees {
      assert(quickEncode(t2)("a".toList) === List(0, 0))
      assert(quickEncode(t2)("b".toList) === List(0, 1))
      assert(quickEncode(t2)("d".toList) === List(1))
      assert(quickEncode(t2)("abd".toList) === List(0, 0, 0, 1, 1))
    }
  }
}
