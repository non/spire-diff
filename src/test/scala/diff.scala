package spire.diff

import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.algebra.Eq
import spire.implicits._

import scala.reflect.ClassTag

trait DiffCheck[A] extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit def arb0: Arbitrary[A]
  implicit def ct0: ClassTag[A]
  implicit def eq0: Eq[A]

  def check(slices: List[Slice], xs: Array[A], ys: Array[A]): Boolean = {
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
    Diff.diff(Array.empty[A], Array.empty[A]) shouldBe Diff(Nil)
  }

  property("diff(xs, xs)") {
    forAll { (xs: Array[A]) =>
      val delta = Diff.diff(xs, xs)
      if (xs.isEmpty) delta.slices shouldBe Nil
      else delta.slices shouldBe List(SliceB(0, 0, xs.length))
    }
  }

  property("diff(xs, nil)") {
    forAll { (xs: Array[A]) =>
      val delta = Diff.diff(xs, Array.empty[A])
      if (xs.isEmpty) delta.slices shouldBe Nil
      else delta.slices shouldBe List(SliceL(0, xs.length))
    }
  }

  property("diff(nil, xs)") {
    forAll { (xs: Array[A]) =>
      val delta = Diff.diff(Array.empty[A], xs)
      if (xs.isEmpty) delta.slices shouldBe Nil
      else delta.slices shouldBe List(SliceR(0, xs.length))
    }
  }

  property("diff(xs, ys) should be ok") {
    forAll { (xs: Array[A], ys: Array[A]) =>
      val delta = Diff.diff(xs, ys)
      check(delta.slices, xs, ys) shouldBe true
    }
  }

  property("diffBody(xs, ys, ...) should be ok") {
    forAll { (xs: Array[A], ys: Array[A]) =>
      val m = Matrix.construct(Chunk(xs), Chunk(ys))
      val slices = m.trace(0, Nil)
      check(slices, xs, ys) shouldBe true
    }
  }
}

class IntDiffCheck extends DiffCheck[Int] {
  val arb0 = implicitly[Arbitrary[Int]]
  val ct0 = implicitly[ClassTag[Int]]
  val eq0 = Eq[Int]
}

class StringDiffCheck extends DiffCheck[String] {
  val arb0 = implicitly[Arbitrary[String]]
  val ct0 = implicitly[ClassTag[String]]
  val eq0 = Eq[String]
}

class CharDiffCheck extends DiffCheck[Char] {
  val arb0 = implicitly[Arbitrary[Char]]
  val ct0 = implicitly[ClassTag[Char]]
  val eq0 = Eq[Char]
}

class ByteDiffCheck extends DiffCheck[Byte] {
  val arb0 = implicitly[Arbitrary[Byte]]
  val ct0 = implicitly[ClassTag[Byte]]
  val eq0 = Eq[Byte]
}
