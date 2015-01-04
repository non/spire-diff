package spire.diff

import spire.algebra.Eq
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.math.{min, max}

import scala.reflect.ClassTag
import scala.annotation.tailrec

object Diff {

  def mkLeft(x0: Int, x1: Int): List[Slice] =
    if (x0 < x1) SliceL(x0, x1) :: Nil else Nil

  def mkRight(x0: Int, x1: Int): List[Slice] =
    if (x0 < x1) SliceR(x0, x1) :: Nil else Nil

  def diff[A: Eq: ClassTag](xs: Array[A], ys: Array[A]): Diff = {
    // some short circuits
    if (xs.length == 0) return Diff(mkRight(0, ys.length))
    if (ys.length == 0) return Diff(mkLeft(0, xs.length))

    // find the first difference
    var i = 0
    while (i < xs.length && i < ys.length && xs(i) === ys(i)) { i += 1 }
    val prefix = if (i > 0) SliceB(0, 0, i) :: Nil else Nil

    // more possible short circuits
    if (xs.length == i) {
      return Diff(prefix ::: mkRight(i, ys.length))
    } else if (ys.length == i) {
      return Diff(prefix ::: mkLeft(i, xs.length))
    }

    // find the last difference
    var j = xs.length - 1
    var k = ys.length - 1
    while (j > i && k > i && xs(j) === ys(k)) { j -= 1; k -= 1 }
    val suffix = if (j < xs.length - 1) SliceB(j + 1, k + 1, xs.length - j - 1) :: Nil else Nil

    // need to diff xs[i..j] with ys[j..k]
    val slices = diffBody(Chunk(xs, i, j + 1), Chunk(ys, i, k + 1), i, suffix)
    Diff(prefix ::: slices)
  }

  def diffBody[A: Eq](xs: Chunk[A], ys: Chunk[A], offset: Int, suffix: List[Slice]): List[Slice] =
    Matrix.construct(xs, ys).trace(offset, suffix)
}

case class Diff(slices: List[Slice]) {

  def display[A: Eq](xs: Array[A], ys: Array[A]): Unit = {
    def loop(slices: List[Slice]): Unit =
      slices match {
        case Nil => ()
        case slice :: rest =>
          slice match {
            case SliceB(x, y, len) =>
              cfor(0)(_ < len, _ + 1) { i =>
                if (xs(x + i) === ys(y + i)) {
                  println("=== " + xs(x + i).toString)
                } else {
                  println("=!= " + xs(x + i).toString + " =!= " + ys(y + i).toString)
                }
              }
            case SliceL(x0, x1) =>
              cfor(x0)(_ < x1, _ + 1) { i => println("<<< " + xs(i)) }
            case SliceR(y0, y1) =>
              cfor(y0)(_ < y1, _ + 1) { i => println(">>> " + ys(i)) }
          }
          loop(rest)
      }
    loop(slices)
  }
}
