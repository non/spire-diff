package spire.diff

/**
 * Chunk represents a window onto an underlying array.
 * 
 * It basically exists to do coordinate translations (and truncation).
 * This allows us to cheaply do a diff on a substring of an array
 * without either copying, or carrying around a ton of indices
 * manually.
 */
case class Chunk[A](data: Array[A], start: Int, limit: Int) {
  def length: Int = limit - start
  def apply(i: Int): A = data(i + start)
}

object Chunk {
  def apply[A](data: Array[A]): Chunk[A] = Chunk(data, 0, data.length)
}
