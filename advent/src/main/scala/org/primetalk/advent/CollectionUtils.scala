package org.primetalk.advent

object CollectionUtils {

  implicit class VectorOps[T](v: Vector[T]) {
    def indexOfSliceAfterN(slice: Vector[T], n: Int): Int = {
      if (n > v.length - slice.length)
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
      if (n > v.length - slice.length)
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
  /** Inserts an element into the beginning of the sorted vector. */
  def insertIntoSortedVector[T: Ordering](v: Vector[T], el: T, prefix: List[T] = List()): Vector[T] = {
    lazy val h = v.head
    if(v.isEmpty)
      (el :: prefix).reverse.toVector
    else if(implicitly[Ordering[T]].gteq(h, el))
      (el :: prefix).foldLeft(v)(_.+:(_))
    else
      insertIntoSortedVector(v.tail, el, h :: prefix)
  }

  /** Insert new elements into the beginning of the sorted vector.
    */
  def insertAllIntoSortedVector[T: Ordering](v: Vector[T], elements: Seq[T], prefix: List[T] = List()): Vector[T] = {
    elements.foldLeft(v)((v, el) => insertIntoSortedVector(v, el))
  }

}

object SequenceUtils {

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

  def unfold[A](z: A)(f: A => Option[A]): A = {
    @annotation.tailrec
    def unfold0(z: A): A = {
      f(z) match {
        case None => z
        case Some(zz) => unfold0(zz)
      }
    }
    unfold0(z)
  }

  def unfoldN[A](z: A, n: Int)(f: A => A): A = {
    @annotation.tailrec
    def unfoldN0(z: A, n: Int): A = {
      if(n == 0)
        z
      else
        unfoldN0(f(z), n - 1)
    }
    unfoldN0(z, n)
  }

  def unfoldWhile[A](z: A)(f: A => A, p: A => Boolean): A = {
    @annotation.tailrec
    def unfoldWhile0(z: A): A = {
      if(p(z))
        unfoldWhile0(f(z))
      else
        z
    }
    unfoldWhile0(z)
  }

  @annotation.tailrec
  final def unfoldUntil[A](f: A => A)(p: A => Boolean)(z: A): A = {
    if(p(z))
      z
    else
      unfoldUntil(f)(p)(f(z))
  }

  /** Starts with initial state `S`, then applies `f` continuously.
    * Each step adds something to output and updates state.
    * When `f` yields `None`, stops.
    *
    * O(N)
    */
  @annotation.tailrec
  final def unfoldWithSuffix[S, T](tail: List[T] = Nil)(f: S => Option[(S, T)])(z: S): List[T] = {
    f(z) match {
      case None => tail
      case Some((zz, b)) => unfoldWithSuffix(b :: tail)(f)(zz)
    }
  }

  def generateStreamFrom[S](s0: S)(f: S => S): Stream[S] =
    s0 #:: generateStreamFrom(f(s0))(f)

  def findFixedPoint[A](z: A)(f: A => A): A = {
    val n = f(z)
    if(n == z)
      z
    else
      findFixedPoint(n)(f)
  }
}
