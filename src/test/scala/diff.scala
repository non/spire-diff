package spire.diff

import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.implicits._

class DiffCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {
  val emptyArray = Array.empty[Int]

  def check[A](slices: List[Slice], xs: Array[A], ys: Array[A]): Boolean = {
    def loop(slices: List[Slice], x: Int, y: Int): Option[(Int, Int)] =
      slices match {
        case Nil =>
          Some((x, y))
        case slice :: tail =>
          slice match {
            case SliceL(x0, x1) =>
              if (x0 == x) loop(tail, x1, y) else None
            case SliceR(y0, y1) =>
              if (y0 == y) loop(tail, x, y1) else None
            case SliceB(x0, y0, len) =>
              if (x0 == x && y0 == y) loop(tail, x + len, y + len) else None
          }
      }
    loop(slices, 0, 0).filter { case (x, y) =>
      x == xs.length && y == ys.length
    }.isDefined
  }

  property("diff(nil, nil)") {
    Diff.diff(emptyArray, emptyArray) shouldBe Diff(Nil)
  }

  property("diff(xs, xs)") {
    forAll { (xs: Array[Int]) =>
      val delta = Diff.diff(xs, xs)
      if (xs.isEmpty) delta.slices shouldBe Nil
      else delta.slices shouldBe List(SliceB(0, 0, xs.length))
    }
  }

  property("diff(xs, nil)") {
    forAll { (xs: Array[Int]) =>
      val delta = Diff.diff(xs, emptyArray)
      if (xs.isEmpty) delta.slices shouldBe Nil
      else delta.slices shouldBe List(SliceL(0, xs.length))
    }
  }

  property("diff(nil, xs)") {
    forAll { (xs: Array[Int]) =>
      val delta = Diff.diff(emptyArray, xs)
      if (xs.isEmpty) delta.slices shouldBe Nil
      else delta.slices shouldBe List(SliceR(0, xs.length))
    }
  }

  property("diff(xs, ys) should be ok") {
    forAll { (xs: Array[Int], ys: Array[Int]) =>
      val delta = Diff.diff(xs, ys)
      check(delta.slices, xs, ys) shouldBe true
    }
  }

  property("diffBody(xs, ys, ...) should be ok") {
    forAll { (xs: Array[Int], ys: Array[Int]) =>
      val slices = Diff.diffBody(Chunk(xs), Chunk(ys), 0, Nil)
      check(slices, xs, ys) shouldBe true
    }
  }
}
