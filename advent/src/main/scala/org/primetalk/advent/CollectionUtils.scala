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

  /**
    * Algorithm for finding a loop.
    *
    * See https://en.wikipedia.org/wiki/Cycle_detection#Floyd%27s_Tortoise_and_Hare
    *
    * @return (loopStart, loopLength)
    */
  def floyd[T](t0: T)(eq: (T, T) => Boolean = (a: T, b: T) => a == b)(f: T => T): (Int, Int) = {

    def goDoubleSpeed(tortoise: T, hare: T, ν: Int): (T, T, Int) = {
      if(eq(tortoise, hare))
        (tortoise, hare, ν)
      else
        goDoubleSpeed(f(tortoise), f(f(hare)), ν + 1)
    }
    val t1 = f(t0)
    val (_, hare, _) = goDoubleSpeed(t1, f(t1), 1)
    def goNormalSpeed(tortoise: T, hare: T, μ: Int): (T, T, Int) = {
      if(eq(tortoise, hare))
        (tortoise, hare, μ)
      else
        goNormalSpeed(f(tortoise), f(hare), μ + 1)
    }
    val (tortoise2, _, μ) = goNormalSpeed(t0, hare, 0)
    def goHare(tortoise: T, hare: T, λ: Int): Int = {
      if(eq(tortoise, hare))
        λ
      else
        goHare(tortoise, f(hare), λ + 1)
    }
    val λ = goHare(tortoise2, f(tortoise2), 1)
    (μ, λ)
  }

}
