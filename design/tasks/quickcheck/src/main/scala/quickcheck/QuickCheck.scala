package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[Int]
    heap <- oneOf(genHeap, const(empty))
  } yield insert(value, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { value: Int =>
    val heap = insert(value, empty)
    findMin(heap) == value
  }

  property("gen1") = forAll { heap: H =>
    val min = if (isEmpty(heap)) 0 else findMin(heap)
    findMin(insert(min, heap)) == min
  }

  property("should return smaller of them when two elements inserted") = forAll { (a: Int, b: Int) =>
    val heap = insert(a, insert(b, empty))
    findMin(heap) == math.min(a, b)
  }

  property("should return empty list when the only element deleted") = forAll { value: Int =>
    val heap = insert(value, empty)
    deleteMin(heap) == empty
  }

  property("should return minimum of one of the two heaps when meld them together") = forAll { (left: H, right: H) =>
    findMin(meld(left, right)) == math.min(findMin(left), findMin(right))
  }

  property("should return sorted sequence when all the elements deleted") = forAll { heap: H =>
    def toList(heap: H): List[Int] = {
      def iterate(heap: H, elements: List[Int]): List[Int] =
        if (isEmpty(heap)) Nil else iterate(deleteMin(heap), findMin(heap) :: elements)
      iterate(heap, Nil)
    }

    val sequence = toList(heap)
    sequence == sequence.sorted
  }

  property("should meld associatively") = forAll { (left: H, right: H) =>
    def equal(left: H, right: H): Boolean =
      if (!isEmpty(left) && !isEmpty(right)) findMin(left) == findMin(right) && equal(deleteMin(left), deleteMin(right))
      else isEmpty(left) && isEmpty(right)

    equal(meld(left, right), meld(deleteMin(left), insert(findMin(left), right)))
  }
}
