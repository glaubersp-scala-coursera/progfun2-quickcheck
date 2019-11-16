package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    oneOf(
      const(empty),
      for {
        x <- arbitrary[A]
        h <- oneOf(const(empty), genHeap)
      } yield insert(x, h)
    )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def toList(h: H): List[A] = {
    if (isEmpty(h)) List()
    else findMin(h) :: toList(deleteMin(h))
  }

  def isSorted(list: List[A]): Boolean = {
    list == list.sorted
  }

  property("insert the minimum element of a heap to the heap itself should keep that element as the minimum ") =
    forAll { (h: H) =>
      val m = if (isEmpty(h)) 0 else findMin(h)
      findMin(insert(m, h)) == m
    }

  property("insert an element into an empty heap should produce the element as the minimum") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("add two elements to empty heap and find the minimum") = forAll { (a: Int, b: Int) =>
    val minVal = if (a <= b) a else b
    val h1 = insert(a, insert(b, empty))
    val h2 = insert(b, insert(a, empty))
    findMin(h1) == minVal && findMin(h2) == minVal
  }

  property("insert into empty heap and delete the minimum") = forAll { (a: Int) =>
    deleteMin(insert(a, empty)) == empty
  }

  property("get a sorted sequence when deleting from any heap") = forAll { (h: H) =>
    isSorted(toList(h))
  }

  property("Finding a minimum of the melding NonEmpty and NonEmpty") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> {
      val min1 = findMin(h1)
      val min2 = findMin(h2)

      val min = findMin(meld(h1, h2))
      if (isEmpty(h1)) min == min2
      else if (isEmpty(h2)) min == min1
      else min == min1 || min == min2
    }
  }
  property("Melding to heaps must keep sorting property") = forAll { (h1: H, h2: H) =>
    val merge = meld(h1, h2)
    isSorted(toList(merge))
  }

  property(
    "The list resulting from melding must have the same ordering as merging the lists from each individual heap") =
    forAll { (h1: H, h2: H) =>
      val merge = meld(h1, h2)
      toList(merge) == (toList(h1) ++ toList(h2)).sorted
    }
}
