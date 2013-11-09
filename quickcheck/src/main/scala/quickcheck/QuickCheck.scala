package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("del one is empty") = forAll { a:Int =>
    val one = insert(a, empty)
    all {
      deleteMin(one) == empty
    }
  }

  property("min returned") = forAll { (a:Int, b:Int) =>
    (a < b) ==> {
      val h1 = insert(b, insert(a, empty))
      val h2 = insert(a, insert(b, empty))
      all(
        findMin(h1) == a,
        findMin(h2) == a
      )
    }
  }

  property("del min") = forAll { (a:Int, b:Int) =>
    (a < b) ==> {
      val h1 = insert(b, insert(a, empty))
      val h2 = insert(a, insert(b, empty))
      val hb = insert(b, empty)
      all(
        deleteMin(h1) == hb,
        deleteMin(h2) == hb
      )
    }
  }

  @tailrec private def asSeq(h:H, sorted:List[Int]):List[Int] = {
    h match {
      case h if h == empty => sorted
      case h => asSeq(deleteMin(h), sorted :+ findMin(h))
    }
  }

  property("returns sorted seq") = forAll { (h:H) =>
    val seq = asSeq(h, List())
    seq == seq.sorted
  }

  property("able to meld heaps") = forAll { (h1:H, h2:H) =>
    val min = List(findMin(h1), findMin(h2)).min
    findMin(meld(h1, h2)) == min
  }


  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
