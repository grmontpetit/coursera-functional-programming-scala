package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val t3 = Fork(Leaf('c', 2), Leaf('d', 3), List('c', 'd'), 5)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("ordering is preserved with the combine function") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    val t = Leaf('t', 3)
    val e = Leaf('e', 2)
    val x = Leaf('x', 4)
    val te = Fork(t, e, List('t', 'e'), 5)
    val xte = Fork(x, te, List('x', 't', 'e'), 9)
    assert(combine(leaflist) === List(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("test the times function") {
    val charList = List('a', 'd', 'f', 'd', 'a', 'a')
    assert(times(charList) == List(('a', 3), ('d', 2), ('f', 1)))
  }

  test("test until function") {
    new TestTrees {
      val trees = List(t1, t3)
      val complexTrees = List(t1, t2, t3)
      val expected = List(Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Fork(Leaf('c', 2), Leaf('d', 3), List('c', 'd'), 5), List('a', 'b', 'c', 'd'), 10))
      val complexExpected = List(Fork(Fork(Leaf('c', 2), Leaf('d', 3), List('c', 'd'), 5), Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9), List('a', 'b', 'a', 'b', 'd'), 14), List('c', 'd', 'a', 'b', 'a', 'b', 'd'), 19))
      assert(until(singleton, combine)(trees) == expected)
      assert(until(singleton, combine)(complexTrees) == complexExpected)

    }
  }

  //        xte(9)
  //       /   \
  //      x(4) te(5)
  //          /   \
  //         /     \
  //        t(3)    e(2)
  test("create a code tree") {
    val chars = List('x', 't', 'e', 'x', 't', 'x', 't', 'e', 't')
    val tree = createCodeTree(chars)
    val expected = Fork(Leaf('x', 4), Fork(Leaf('t', 3), Leaf('e', 2), List('t', 'e'), 5), List('x', 't', 'e'), 9)
    assert(tree == expected)
  }

}
