package org.primetalk.advent.tools

object SequenceUtils {

  /** Sequence generating function. */
  type SequenceGen[T] = T => T
  type FiniteSequenceGen[T] = T => Option[T]
  /**
    * Algorithm for finding a loop in a sequence..
    *
    * See https://en.wikipedia.org/wiki/Cycle_detection#Floyd%27s_Tortoise_and_Hare
    *
    * @return (loopStart, loopLength)
    */
  def floyd[T](t0: T)(eq: (T, T) => Boolean = (a: T, b: T) => a == b)(f: SequenceGen[T] ): (Int, Int) = {

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

  def unfold[A](z: A)(gen: FiniteSequenceGen[A]): A = {
    @annotation.tailrec
    def unfold0(z: A): A = {
      gen(z) match {
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
      if(p(z)) {
        val next = f(z)
        if(next == z) throw new IllegalStateException("Stuck at " + z)
        unfoldWhile0(next)
      } else
        z
    }
    unfoldWhile0(z)
  }

  @annotation.tailrec
  final def unfoldUntil[A](f: A => A)(p: A => Boolean)(z: A): A = {
    if(p(z))
      z
    else {
      val next = f(z)
      if(next == z) throw new IllegalStateException("Stuck at " + z)
      unfoldUntil(f)(p)(next)
    }
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

  /** Finds minimum argument for predicate to hold true. */
  def findMinArgForPredicate(p: Int => Boolean)(guessBelow: Int, guessAbove: Int): Int = {
    if(guessAbove == guessBelow + 1)
      guessAbove
    else {
      val next = (guessAbove + guessBelow) / 2
      if(p(next))
        findMinArgForPredicate(p)(guessBelow, next)
      else
        findMinArgForPredicate(p)(next, guessAbove)
    }
  }

}
