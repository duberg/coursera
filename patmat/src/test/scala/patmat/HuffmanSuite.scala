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


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
    assert(times(List('a', 'b', 'a', 'a')) === List(('a', 3), ('b', 1)))
    assert(times(List('a', 'b', 'a', 'a', 'c')) === List(('a', 3), ('b', 1), ('c', 1)))
    assert(times(List()) === Nil)
    assert(times(List('d')) === List(('d', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("makeOrderedLeafList for some frequency table2") {
    assert(makeOrderedLeafList(List(('b', 2), ('c', 1), ('e', 3), ('d', 1))) === List(Leaf('c',1), Leaf('d',1), Leaf('b',2), Leaf('e',3)))
  }

  test("makeOrderedLeafList for some frequency table3") {
    assert(makeOrderedLeafList(List()) === Nil)
  }

  test("makeOrderedLeafList for some frequency table4") {
    assert(makeOrderedLeafList(List(('a', 2), ('b', 1), ('c', 3), ('d', 1), ('e', 0))) === List(Leaf('e',0), Leaf('b',1), Leaf('d',1), Leaf('a',2), Leaf('c',3)))
  }

  test("combine ef") {
    val leaflist = List(Leaf('f', 1), Leaf('e', 1))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('f',1),List('e', 'f'),2)))
  }

  test("combine efgh") {
    val leaflist = List(Leaf('e', 1), Leaf('f', 1), Leaf('h', 1), Leaf('g', 1))
    val leaflistc1 = combine(leaflist)
    val leaflistc2 = combine(leaflistc1)
    val leaflistc3 = combine(leaflistc2)
    assert(leaflistc1 === List(Leaf('h',1), Leaf('g',1), Fork(Leaf('e',1),Leaf('f',1),List('e', 'f'),2)))
    assert(leaflistc3 === List(
      Fork(
        Fork(Leaf('e',1),Leaf('f',1),List('e', 'f'),2),
        Fork(Leaf('g',1),Leaf('h',1),List('g', 'h'),2),
        List('e', 'f', 'g', 'h'),
        4
      )
    ))
  }

  test("combine of some leaf list2") {
    val leaflist = List(Leaf('e', 4), Leaf('t', 6), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x', 4), Fork(Leaf('e', 4),Leaf('t', 6),List('e', 't'), 10)))
  }

  test("createCodeTree") {
    val chrs = List('e', 'f', 'a', 'b', 'a', 'a', 'b', 'g', 'h', 'a', 'a', 'a', 'a', 'a', 'b', 'c', 'd')
    assert(createCodeTree(chrs) ==
      Fork(
        Leaf('a', 8),
        Fork(
          Fork(
            Leaf('b', 3),
            Fork(Leaf('c', 1), Leaf('d', 1), List('c', 'd'), 2),
            List('b', 'c', 'd'),
            5
          ),
          Fork(
            Fork(Leaf('e', 1), Leaf('f', 1), List('e', 'f'), 2),
            Fork(Leaf('g', 1), Leaf('h', 1), List('g', 'h'),2),
            List('e', 'f', 'g', 'h'),
            4
          ),
          List('b', 'c', 'd', 'e', 'f', 'g', 'h'),
          9
        ),
        List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
        17
      )
    )
  }

  test("decode") {
    val chrs = List('e', 'f', 'a', 'b', 'a', 'a', 'b', 'g', 'h', 'a', 'a', 'a', 'a', 'a', 'b', 'c', 'd')
    val tree = createCodeTree(chrs)
    assert(decode(tree, List(1,0,0,0,1,0,1,0)) == List('b','a','c'))
  }

  test("decode and encode a very short text 3should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode") {
    val chrs = List('e', 't', 'e', 'e', 't', 't', 't', 'e', 'g', 'h', 't', 't', 't', 's', 'e', 'x', 't', 't')
    val tree = createCodeTree(chrs)
    assert(decode(tree, encode(tree)("testtset".toList)) === "testtset".toList)
  }

  test("codeBits") {
    val codeTable: CodeTable = List(('a', List(1, 0, 0)), ('b', List(1, 1, 1)))
    assert(codeBits(codeTable)('b') == List(1, 1, 1))
    assert(codeBits(codeTable)('a') == List(1, 0, 0))
  }

  test("convert") {
    val chrs = List('e', 'f', 'a', 'b', 'a', 'a', 'b', 'g', 'h', 'a', 'a', 'a', 'a', 'a', 'b', 'c', 'd')
    assert(convert(createCodeTree(chrs)) == List(
      ('a', List(0)),
      ('b', List(1, 0, 0)),
      ('c', List(1, 0, 1, 0)),
      ('d', List(1, 0, 1, 1)),
      ('e', List(1, 1, 0, 0)),
      ('f', List(1, 1, 0, 1)),
      ('g', List(1, 1, 1, 0)),
      ('h', List(1, 1, 1, 1))
    ))
  }

  test("quickEncode") {
    val chrs = List('e', 'f', 'a', 'b', 'a', 'a', 'b', 'g', 'h', 'a', 'a', 'a', 'a', 'a', 'b', 'c', 'd')
    val tree = createCodeTree(chrs)
    assert(quickEncode(tree)("bbaaffhh".toList) == List(
      1,0,0,
      1,0,0,
      0,
      0,
      1,1,0,1,
      1,1,0,1,
      1,1,1,1,
      1,1,1,1
    ))
  }
}
