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

  //        abcde(9)
  //       /     \
  //      a(5)  bcde(4)
  //           /    \
  //          /      \
  //         /        \
  //        /          \
  //       bc(2)       de(2)
  //      /  \        /  \
  //     /    \      /    \
  //    b(1)   c(1) d(1)   e(1)
  test("create a code tree") {
    val bc = Fork(Leaf('b', 1), Leaf('c', 1), List('b', 'c'), 2)
    val de = Fork(Leaf('d', 1), Leaf('e', 1), List('d', 'e'), 2)
    val bcde = Fork(bc, de, List('b', 'c', 'd', 'e'), 4)
    val a = Leaf('a', 5)
    val chars = List('a', 'b', 'c', 'd', 'e')
    val expected = Fork(a, bcde, chars, 9)
    val tree = createCodeTree(chars)
    assert(tree == expected)
  }


}
