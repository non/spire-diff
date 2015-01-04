package spire.diff

import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.implicits._

class DiffCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {
  val emptyArray = Array.empty[Int]

  // property("diff(nil, nil)") {
  //   Diff.getSlices(emptyArray, emptyArray) shouldBe Nil
  // }
  // 
  // property("diff(xs, xs)") {
  //   forAll { (xs: Array[Int]) => if (xs.nonEmpty)
  //     Diff.getSlices(xs, xs) shouldBe List(SliceB(0, 0, xs.length))
  //   }
  // }
  // 
  // property("diff(xs, nil)") {
  //   forAll { (xs: Array[Int]) => if (xs.nonEmpty)
  //     Diff.getSlices(xs, emptyArray) shouldBe List(SliceL(0, xs.length))
  //   }
  // }

  property("diff seems ok") {
    val xs = Array("one", "two", "three", "four", "five", "six")
    val ys = Array("zero", "one", "one", "two", "three", "five")
    // val xs = Array("one", "two", "three")
    // val ys = Array("four", "five", "six")
    //Diff.printDiff(xs, ys, Diff.getSlices(xs, ys))
    val slices = Diff.getDiff(Chunk(xs), Chunk(ys), Nil)
    Diff.printDiff(xs, ys, slices)
  }

  // property("diff(ys, xs:ys:zs)") {
  //   forAll { (xs: Int, ys: Array[Int], zs: Array[Int]) =>
  //     if (xs.nonEmpty && ys.nonEmpty && zs.nonEmpty) {
  //       val (xl, yl, zl) = (xs.length, ys.length, zs.length)
  //       val left = ys
  //       val right = xs ++ ys ++ zs
  //       val diff = Diff.getSlices(left, right)
  //       val expected = SliceR(0, xl) :: SliceB(0, xl, yl) :: SliceR(xl + yl, xl + yl + zl) :: Nil
  //       diff shouldBe expected
  //     }
  //   }
  // }
}
