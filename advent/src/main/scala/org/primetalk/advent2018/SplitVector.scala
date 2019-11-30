package org.primetalk.advent2018

/** Vector that is split at some position. */
case class SplitVector[T](left: Vector[T], right: Vector[T]) {
  def splitPosition: Int =
    left.size

  def size: Int =
    left.size + right.size


  /** This is especially effective when `n` is near the splitPosition.
    * O(|n-splitPosition|)
    */
  def drop(n: Int): SplitVector[T] = {
    val m = n - left.size
    if (m >= 0) {
      SplitVector(Vector(), right.drop(m))
    } else {
      SplitVector(left.takeRight(-m), right)
    }
  }

  def take(n: Int): SplitVector[T] = {
    val m = n - left.size
    if (m >= 0) {
      SplitVector(left, right.take(m))
    } else {
      SplitVector(left.dropRight(-m), Vector())
    }
  }


  def splitAt(n: Int): (SplitVector[T], SplitVector[T]) = {
    val m = n - left.size
    if (m >= 0) {
      val (t,d) = right.splitAt(m)
      (SplitVector(left, t), SplitVector(Vector(), d))
    } else {
      val (t,d) = left.splitAt(n)
      (SplitVector(t, Vector()), SplitVector(d, right))
    }
  }




  def :+(el: T) =
    SplitVector(left, right :+ el)

  def +:(el: T) =
    SplitVector(el +: left, right)

  /** Combines the two split vectors in such a way that the new vector has split position in between. */
  def ++(other: SplitVector[T]): SplitVector[T] =
    SplitVector(
      left ++ right, other.left ++ other.right
    )


  def head: T =
    if(left.nonEmpty)
      left.head
    else
      right.head

  def tail: SplitVector[T] =
    if(left.nonEmpty)
      SplitVector(left.tail, right)
    else
      SplitVector(left, right.tail)


  def toVector: Vector[T] =
    left ++ right

  def map[A](f: T => A): SplitVector[A] =
    SplitVector(left.map(f), right.map(f))

}

object SplitVector {
  def empty[T]: SplitVector[T] =
    SplitVector(Vector(), Vector())
}