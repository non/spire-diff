package spire.diff

import spire.algebra.Eq
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.math.{min, max}

import scala.reflect.ClassTag
import scala.annotation.tailrec

case class Chunk[A](data: Array[A], start: Int, limit: Int) {
  def length: Int = limit - start
  def apply(i: Int): A = data(i + start)
}

object Chunk {
  def apply[A](data: Array[A]): Chunk[A] = Chunk(data, 0, data.length)
}

sealed trait Slice
case class SliceB(startL: Int, startR: Int, len: Int) extends Slice
case class SliceL(startL: Int, limitL: Int) extends Slice
case class SliceR(startR: Int, limitR: Int) extends Slice

sealed abstract class Marker(val glyph: Char) { def size: Int }
case object Terminal extends Marker('∅') { def size: Int = 0 }
case class Match(size: Int) extends Marker('↖')
case class West(size: Int) extends Marker('←')
case class North(size: Int) extends Marker('↑')
case class Split(size: Int) extends Marker('┛')

object Diff {

  def diffBody[A: Eq](xs: Chunk[A], ys: Chunk[A], offset: Int, suffix: List[Slice]): List[Slice] = {
    val matrix = Matrix.construct(xs, ys)
    //matrix.display()
    matrix.trace(offset, suffix)
  }

  def diff[A: Eq: ClassTag](xs: Array[A], ys: Array[A]): Diff = {
    // some short circuits
    if (xs.length == 0) {
      return Diff(if (ys.length == 0) Nil else SliceR(0, ys.length) :: Nil)
    } else if (ys.length == 0) {
      return Diff(SliceL(0, xs.length) :: Nil)
    }

    // find the first difference
    var i = 0
    while (i < xs.length && i < ys.length && xs(i) === ys(i)) { i += 1 }
    val prefix = if (i > 0) Some(SliceB(0, 0, i)) else None

    // more possible short circuits
    if (xs.length == i) {
      prefix match {
        case Some(p) =>
          Diff(if (ys.length == i) p :: Nil else p :: SliceR(i, ys.length) :: Nil)
        case None =>
          Diff(if (ys.length == i) Nil else SliceR(i, ys.length) :: Nil)
      }
    } else if (ys.length == i) {
      prefix match {
        case Some(p) =>
          return Diff(p :: SliceL(i, xs.length) :: Nil)
        case None =>
          return Diff(SliceL(i, xs.length) :: Nil)
      }
    }

    // find the last difference
    var j = xs.length - 1
    var k = ys.length - 1
    while (j > i && k > i && xs(j) === ys(k)) { j -= 1; k -= 1 }
    val suffix = if (j < xs.length - 1) SliceB(j + 1, k + 1, xs.length - j - 1) :: Nil else Nil

    // need to diff xs[i..j] with ys[j..k]
    val slices = diffBody(Chunk(xs, i, j + 1), Chunk(ys, i, k + 1), i, suffix)
    prefix  match {
      case Some(p) => Diff(p :: slices)
      case None => Diff(slices)
    }
  }
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

    def buildLeft(x: Int, y: Int, x1: Int): (Int, Int, Slice) =
      this(x, y) match {
        case West(_) | Split(_) => buildLeft(x - 1, y, x1)
        case _ => (x, y, SliceL(offset + x, offset + x1))
      }

    def buildRight(x: Int, y: Int, y1: Int): (Int, Int, Slice) =
      this(x, y) match {
        case North(_) | Split(_) => buildRight(x, y - 1, y1)
        case _ => (x, y, SliceR(offset + y, offset + y1))
      }

    def buildBoth(x: Int, y: Int, len: Int): (Int, Int, Slice) =
      this(x, y) match {
        case Match(_) => buildBoth(x - 1, y - 1, len + 1)
        case _ => (x, y, SliceB(offset + x, offset + y, len))
      }

    if (rows.nonEmpty) {
      loop(rows.last.length - 1, rows.length - 1, suffix)
    } else Nil
  }
}
