package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, insert(v, h))
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    findMin(insert(b, insert(a, empty))) == Seq(a, b).min
  }

  property("del") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  /**
    * Given any heap, you should get a sorted sequence of
    * elements when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("gen2") = forAll { (h: H) =>
    @tailrec
    def rec(h: H, s: Seq[A]): Seq[A] =
      if (isEmpty(h)) s else rec(deleteMin(h), s :+ findMin(h))
    val s = rec(h, Seq.empty)
    s == s.sorted
  }

  /**
    * Finding a minimum of the melding of any two heaps should return
    * a minimum of one or the other.
    */
  property("gen3") = forAll { (h1: H, h2: H) =>
   findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("gen4") = forAll { h: H =>
    findMin(deleteMin(h)) == findMin(deleteMin(h))
  }

  property("findelem") = forAll { (a: Int, h: H) =>
    @tailrec
    def find(h: H, elem: A): Boolean =
      if (isEmpty(h)) false
      else if (findMin(h) == elem) true else find(deleteMin(h), elem)
    find(insert(a, h), a)
  }
}
