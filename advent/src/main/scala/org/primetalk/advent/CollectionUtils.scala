package org.primetalk.advent

object CollectionUtils {

  implicit class VectorOps[T](v: Vector[T]) {
    def indexOfSliceAfterN(slice: Vector[T], n: Int): Int = {
      if(n > v.length - slice.length)
        -1
      else {
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
      if(n > v.length - slice.length)
        -1
      else {
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
}
