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
    val t3 = Fork(Leaf('e', 2), Leaf('f', 3), List('e', 'f'), 5)
  }

  trait TreeEtxz {
    //               _______________________
    // Letter        | z  | x  | t   | e   |
    // Frequency (K) | 10 | 4  | 3   | 2   |
    // Bits          | 0  | 00 | 011 | 010 |
    //               -----------------------
    //        zxte(19)
    //       /    \
    //     etx(9)  z(10)
    //    /     \
    //   x(4)   et(5)
    //          /   \
    //         e(2)  t(3)
    val t = Leaf('t', 3)
    val e = Leaf('e', 2)
    val x = Leaf('x', 4)
    val z = Leaf('z', 10)
    val et = Fork(e, t, List('e', 't'), 5)
    val etx = Fork(x, et, List('e','t','x'), 9)
    val etxz = Fork(etx, z, List('e','t','x','z'), 19)
  }

  trait TreeAbcdef {
    //               _____________________________________
    // Letter        | A  | B  |  C  |  D  |  E   | F    |
    // Frequency (K) | 10 | 7  | 5   |  4  | 2    | 1    |
    // Bits          | 11 | 10 | 00  | 011 | 0101 | 0100 |
    //               -------------------------------------
    //          ABCDEF(29)
    //         /       \
    //       CDEF(12)  AB(17)
    //      /    \    /  \
    //     /      \  B(7) A(10)
    //    C(5)     \
    //             DEF(12)
    //            /   \
    //           EF(3) D(4)
    //          /  \
    //         F(1) E(2)
    val a = Leaf('A', 10)
    val b = Leaf('B', 7)
    val c = Leaf('C', 5)
    val d = Leaf('D', 4)
    val e = Leaf('E', 2)
    val f = Leaf('F', 1)
    val ab = Fork(b, a, List('A', 'B'), 17)
    val ef = Fork(f, e, List('E', 'F'), 3)
    val `def` = Fork(ef, d, List('D', 'E', 'F'), 7)
    val cdef = Fork(c, `def`, List('C', 'D', 'E', 'F'), 12)
    val abcdef = Fork(cdef, ab, List('A','B','C','D','E','F'), 29)
  }

  trait TreeEtx {
    //               __________________
    // Letter        | x  |  t  |  e  |
    // Frequency (K) | 4  | 3   | 2   |
    // Bits          | 0  | 101 | 100 |
    //               ------------------
    //        etx(9)
    //       /   \
    //      x(4) et(5)
    //          /   \
    //         /     \
    //        e(2)    t(3)
    val etx = Fork(Leaf('x',4),Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5),List('e', 't', 'x'),9)
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
    val te = Fork(e, t, List('e', 't'), 5)
    assert(combine(leaflist) === List(x, te))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode BDE with abcdef") {
    new TreeAbcdef {
      val expected = List(1, 0, 0, 1, 1, 0, 1, 0, 1)
      assert(encode(abcdef)(List('B','D','E')) == expected)
    }
  }

  test("decode a few chars from the ABCDEF tree") {
    new TreeAbcdef {
      assert(decode(abcdef, List(1, 1, 0, 1, 1, 0, 1, 0, 1)) == "ADE".toList)
      assert(decode(abcdef, List(0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1)) == "FECD".toList)
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
      val expected = List(Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Fork(Leaf('e', 2), Leaf('f', 3), List('e', 'f'), 5), List('a', 'b', 'e', 'f'), 10))
      assert(until(singleton, combine)(trees) == expected)
    }
  }

  test("test with tree etxz") {
    new TreeEtxz {
      val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('z', 10))
      assert(until(singleton, combine)(leaflist) === List(etxz))
    }
  }

  test("test with tree etx") {
    new TreeEtx {
      val chars = List('x', 't', 'e', 'x', 't', 'x', 't', 'e', 'x')
      val tree = createCodeTree(chars)
      assert(tree === etx)
    }
  }

  test("test tree abcdef") {
    new TreeAbcdef {
      val chars = List('A','A','A','A','A','A','A','A','A','A',
        'B','B','B','B','B','B','B',
        'C','C','C','C','C',
        'D','D','D','D',
        'E','E',
        'F')
      val tree = createCodeTree(chars)
      assert(tree === abcdef)
    }
  }

  test("decode secret") {
    assert(decodedSecret === "huffmanestcool".toList)
  }

  test("build CodeTable with ABCDEF") {
    new TreeAbcdef {
      val expected = List(('A',List(1, 1)), ('B',List(1, 0)), ('C',List(0, 0)),
        ('D',List(0, 1, 1)), ('E',List(0, 1, 0, 1)), ('F',List(0, 1, 0, 0)))
      assert(convert(abcdef).sortWith(_._1 < _._1) === expected)
    }
  }

  test("test the merging of CodeTables") {
    val expected = List(('A',List(1, 1)), ('B',List(1, 0)), ('C',List(0, 0)),
      ('D',List(0, 1, 1)), ('E',List(0, 1, 0, 1)), ('F',List(0, 1, 0, 0)))
    val table1 = List(('A',List(1, 1)), ('B',List(1, 0)), ('C',List(0, 0)))
    val table2 = List(('D',List(0, 1, 1)), ('E',List(0, 1, 0, 1)), ('F',List(0, 1, 0, 0)))
    assert(mergeCodeTables(table1, table2) === expected)
  }

  test("test the quick encode") {
    new TreeAbcdef {
      val text = List('C', 'D', 'F')
      assert(quickEncode(abcdef)(text) === List(0, 0, 0, 1, 1, 0, 1, 0, 0))
    }
  }

  // The sample tree given in the assignment page is wrong
  ignore("create a full test tree") {
    val chars = List('a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    val tree = createCodeTree(chars)
    val a = Leaf('a', 8)
    val b = Leaf('b', 3)
    val c = Leaf('c', 1)
    val d = Leaf('d', 1)
    val e = Leaf('e', 1)
    val f = Leaf('f', 1)
    val g = Leaf('g', 1)
    val h = Leaf('h', 1)
    val gh = Fork(g, h, List('g', 'h'), 2)
    val ef = Fork(e, f, List('e', 'f'), 2)
    val cd = Fork(g, h, List('c', 'd'), 2)
    val bcd = Fork(b, cd, List('b','c','d'), 5)
    val efgh = Fork(ef, gh, List('e','f','g','h'), 4)
    val bcdefgh = Fork(bcd, efgh, List('b','c','d','e','f','g'), 9)
    val expected = Fork(a, bcdefgh, List('a','b','c','d','e','f','g','h'), 17)
    assert(tree === expected)
  }

}
