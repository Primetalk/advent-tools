package org.primetalk.advent.tools

object CollectionUtils {

  implicit class VectorOps[T](v: Vector[T]) {
    def indexOfSliceAfterN(slice: Vector[T], n: Int): Int = {
      if (n > v.length - slice.length)
        -1
      else {
        @scala.annotation.tailrec
        def compare(pos: Int, i: Int): Boolean = {
          i == slice.length || v(pos) == slice(i) && compare(pos + 1, i + 1)
        }

        if (compare(n, 0))
          n
        else
          indexOfSliceAfterN(slice, n + 1)
      }
    }
  }

  implicit class IndexedSeqOps[T](v: IndexedSeq[T]) {
    def indexOfSliceAfterN(slice: IndexedSeq[T], n: Int): Int = {
      if (n > v.length - slice.length)
        -1
      else {
        @scala.annotation.tailrec
        def compare(pos: Int, i: Int): Boolean = {
          i == slice.length || v(pos) == slice(i) && compare(pos + 1, i + 1)
        }

        if (compare(math.max(0, n), 0))
          n
        else
          indexOfSliceAfterN(slice, n + 1)
      }
    }
  }
  /** Inserts an element into the beginning of the sorted vector. */
  @scala.annotation.tailrec
  def insertIntoSortedVector[T: Ordering](v: Vector[T], el: T, prefix: List[T] = List()): Vector[T] = {
    lazy val h = v.head
    if(v.isEmpty)
      (el :: prefix).reverse.toVector
    else if(implicitly[Ordering[T]].gteq(h, el))
      (el :: prefix).foldLeft(v)(_.+:(_))
    else
      insertIntoSortedVector(v.tail, el, h :: prefix)
  }

  /** Inserts an element into the beginning of the sorted vector. */
  @scala.annotation.tailrec
  def insertIntoSortedList[T: Ordering](v: List[T], el: T, prefix: List[T] = List()): List[T] = v match {
    case Nil =>
      (el :: prefix).reverse
    case h :: tail =>
      if(implicitly[Ordering[T]].gteq(h, el))
        prefix reverse_::: el :: v
      else
        insertIntoSortedList(tail, el, h :: prefix)
  }

  /** Insert new elements into the beginning of the sorted vector.
    */
  def insertAllIntoSortedVector[T: Ordering](v: Vector[T], elements: Seq[T], prefix: List[T] = List()): Vector[T] = {
    elements.foldLeft(v)((v, el) => insertIntoSortedVector(v, el))
  }

  def reverseOrder[T](implicit ordering: Ordering[T]): Ordering[T] =
    ordering.reverse

  def descending[T](implicit ordering: Ordering[T]): Ordering[T] =
    ordering.reverse

}
