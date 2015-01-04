package spire.diff

/**
 * Marker represents an entry in the "diff matrix".
 * 
 * Diff finds the longest common substring to find the "best" delta
 * between the two sources. To do this, it constructs a matrix which
 * tracks the possible ways of slicing up the data.
 * 
 * Terminal means we have fully consumed a source.
 * 
 * West, North, and Match correspond to consume from the left, right,
 * and both sources (respectively).
 * 
 * Split means we could consume from either source.
 */
sealed abstract class Marker(val glyph: Char) { def size: Int }

case object Terminal extends Marker('∅') { def size: Int = 0 }
case class Match(size: Int) extends Marker('↖')
case class West(size: Int) extends Marker('←')
case class North(size: Int) extends Marker('↑')
case class Split(size: Int) extends Marker('┛')
