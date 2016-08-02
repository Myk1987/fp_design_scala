package quickcheck

import org.scalacheck.Prop._
import org.scalacheck._
import scala.math._


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genEmptyHeap:Gen[H] = empty
  lazy val genNonEmptyHeap:Gen[H] = for(
    a <- Arbitrary.arbitrary[A];
    h <- Gen.oneOf( genEmptyHeap , genNonEmptyHeap)
  ) yield insert(a, h)

  lazy val genHeap: Gen[H] = Gen.oneOf(genEmptyHeap , genNonEmptyHeap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("meld_empty") = forAll { (h1: H) =>
    (meld(h1, empty) == h1 /*:| "nonEmpty meld Empty"*/ &&
    meld(empty, h1) == h1 /*:| "Empty meld nonEmpty"*/)
  }


  property("find_min") = forAll{ (a:A, b:A) =>
    findMin( insert(b, insert(a, empty))) == min(a,b)
  }

  property("insert_delete") = forAll{ (a:A) =>
    deleteMin( insert(a, empty)) == empty
  }

  property("deletion_ordering") = forAll{ h:H =>
    def deleteAll(h:H):List[A] =
      if(isEmpty(h)) List[A]()
      else findMin(h) :: deleteAll(deleteMin(h))
    val l = deleteAll(h)
    l == l.sorted(ord)
  }

  property("meld") = forAll { (h1:H, h2:H) =>
    val h3 = meld(h1,h2)
    (isEmpty(h3) ||
      (isEmpty(h1) && findMin(h2) == findMin(h3)) ||
      (isEmpty(h2) && findMin(h1) == findMin(h3)) ||
      (min(findMin(h1), findMin(h2)) == findMin(h3))
      )
  }

/*
* If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  Finding a minimum of the melding of any two heaps should return a minimum of one or the other.

* */



}
