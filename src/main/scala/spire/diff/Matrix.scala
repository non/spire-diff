package spire.diff

import spire.algebra.Eq
import spire.syntax.cfor._
import spire.syntax.eq._
import scala.annotation.tailrec

/**
 * Matrix represents all the possible ways to consume the data in the
 * left and right sources.
 * 
 * The class is used to determine the best path to take. This path
 * will contain one of the longest-common substrings of input (i.e. no
 * other path will find more places where input can be pulled from
 * both sources simultaneously).
 */
case class Matrix(rows: Array[Array[Marker]]) {

  def apply(x: Int, y: Int): Marker = rows(y)(x)

  def maxSize: Int =
    if (rows.nonEmpty) rows.last.last.size else 0

  def display(): Unit = {
    val width = maxSize.toString.length
    val fmt = "%c{%" + width + "d} "
    rows.foreach { row =>
      row.foreach { m =>
        print(fmt format (m.glyph, m.size))
      }
      println("")
    }
  }

  /**
   * Trace the path through the matrix (from the southeast corner to
   * the northwest) that contains one of the longest-common
   * substrings.
   * 
   * The southeast corner represents having consumed all input from
   * both sources. The northwest corner reprsents having consumed no
   * input yet.
   * 
   * Moving west means consuming input from the left source, and
   * moving north represents consuming input from the right
   * source. Moving northwest means consuming input from both sources.
   */
  def trace(offset: Int, suffix: List[Slice]): List[Slice] = {
    @tailrec def loop(x: Int, y: Int, slices: List[Slice]): List[Slice] = {
      this(x, y) match {
        case Terminal =>
          if (y > 0) SliceR(offset, offset + y) :: slices
          else if (x > 0) SliceL(offset, offset + x) :: slices
          else slices
        case Match(_) =>
          val (xx, yy, slice) = buildBoth(x - 1, y - 1, 1)
          loop(xx, yy, slice :: slices)
        case West(_) =>
          val (xx, yy, slice) = buildLeft(x - 1, y, x)
          loop(xx, yy, slice :: slices)
        case North(_) | Split(_) =>
          val (xx, yy, slice) = buildRight(x, y - 1, y)
          loop(xx, yy, slice :: slices)
      }
    }

    // start consuming input from the left source, as long as it is
    // reasonable to do so.
    def buildLeft(x: Int, y: Int, x1: Int): (Int, Int, Slice) =
      this(x, y) match {
        case West(_) | Split(_) => buildLeft(x - 1, y, x1)
        case _ => (x, y, SliceL(offset + x, offset + x1))
      }

    // start consuming input from the right source, as long as it is
    // reasonable to do so.
    def buildRight(x: Int, y: Int, y1: Int): (Int, Int, Slice) =
      this(x, y) match {
        case North(_) | Split(_) => buildRight(x, y - 1, y1)
        case _ => (x, y, SliceR(offset + y, offset + y1))
      }

    // start consuming input from both sources, as long as it is
    // reasonable to do so.
    def buildBoth(x: Int, y: Int, len: Int): (Int, Int, Slice) =
      this(x, y) match {
        case Match(_) => buildBoth(x - 1, y - 1, len + 1)
        case _ => (x, y, SliceB(offset + x, offset + y, len))
      }

    if (rows.isEmpty) Nil else {
      loop(rows.last.length - 1, rows.length - 1, suffix)
    }
  }
}

object Matrix {
  def construct[A: Eq](xs: Chunk[A], ys: Chunk[A]): Matrix = {
    val lx = xs.length
    val ly = ys.length
    val rows = Array.fill[Marker](ly + 1, lx + 1)(Terminal)

    cfor(1)(_ <= ly, _ + 1) { y =>
      cfor(1)(_ <= lx, _ + 1) { x =>
        rows(y)(x) =
          if (ys(y - 1) === xs(x - 1)) {
            Match(rows(y - 1)(x - 1).size + 1)
          } else {
            val n = rows(y - 1)(x).size
            val w = rows(y)(x - 1).size
            if (n < w) West(w) else if (w < n) North(w) else Split(w)
          }
      }
    }
    Matrix(rows)
  }
}
