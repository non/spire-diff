package spire.diff

/**
 * Slice represents an atomic delta between two sources.
 * 
 * To be more specific, a slice represents taking one or more
 * elemtents from the left, right, or both sources.
 */
sealed trait Slice

case class SliceB(startL: Int, startR: Int, len: Int) extends Slice
case class SliceL(startL: Int, limitL: Int) extends Slice
case class SliceR(startR: Int, limitR: Int) extends Slice
