package org.primetalk.advent.tools

import scala.annotation.tailrec

//noinspection NonAsciiCharacters
object SequenceUtils {

  /** Sequence generating function. */
  type SequenceGen[T] = T => T
  type FiniteSequenceGen[T] = T => Option[T]
  type Predicate[T] = T => Boolean
  /**
    * Algorithm for finding a loop in a sequence..
    *
    * See https://en.wikipedia.org/wiki/Cycle_detection#Floyd%27s_Tortoise_and_Hare
    *
    * @return (loopStart, loopLength)
    */
  def floydInt[T](t0: T)(eq: (T, T) => Boolean = (a: T, b: T) => a == b)(f: SequenceGen[T] ): (Int, Int) = {

    @tailrec
    def goDoubleSpeed(tortoise: T, hare: T, ν: Int): (T, T, Int) = {
      if(eq(tortoise, hare))
        (tortoise, hare, ν)
      else
        goDoubleSpeed(f(tortoise), f(f(hare)), ν + 1)
    }
    val t1 = f(t0)
    val (_, hare, _) = goDoubleSpeed(t1, f(t1), 1)
    @tailrec
    def goNormalSpeed(tortoise: T, hare: T, μ: Int): (T, T, Int) = {
      if(eq(tortoise, hare))
        (tortoise, hare, μ)
      else
        goNormalSpeed(f(tortoise), f(hare), μ + 1)
    }
    val (tortoise2, _, μ) = goNormalSpeed(t0, hare, 0)
    @tailrec
    def goHare(tortoise: T, hare: T, λ: Int): Int = {
      if(eq(tortoise, hare))
        λ
      else
        goHare(tortoise, f(hare), λ + 1)
    }
    val λ = goHare(tortoise2, f(tortoise2), 1)
    (μ, λ)
  }

  final def floyd[T](t0: T)(eq: (T, T) => Boolean = (a: T, b: T) => a == b)(f: SequenceGen[T] ): (Long, Long) = {

    @tailrec
    def goDoubleSpeed(tortoise: T, hare: T, ν: Long): (T, T, Long) = {
      if(eq(tortoise, hare))
        (tortoise, hare, ν)
      else
        goDoubleSpeed(f(tortoise), f(f(hare)), ν + 1)
    }
    val t1 = f(t0)
    val (_, hare, _) = goDoubleSpeed(t1, f(t1), 1)
    @tailrec
    def goNormalSpeed(tortoise: T, hare: T, μ: Long): (T, T, Long) = {
      if(eq(tortoise, hare))
        (tortoise, hare, μ)
      else
        goNormalSpeed(f(tortoise), f(hare), μ + 1)
    }
    val (tortoise2, _, μ) = goNormalSpeed(t0, hare, 0)
    @tailrec
    def goHare(tortoise: T, hare: T, λ: Long): Long = {
      if(eq(tortoise, hare))
        λ
      else
        goHare(tortoise, f(hare), λ + 1)
    }
    val λ = goHare(tortoise2, f(tortoise2), 1)
    (μ, λ)
  }

  // states should contain 3 elements.
  // [0] - initial state
  // [1] - tortoise - any value
  // [2] - hare - any value
  @inline
  def floydMutable[T](states: Array[T])(eq: (T, T) => Boolean = (a: T, b: T) => a == b, copy: (T, T) => Unit)(update: T => Unit): (Long, Long) = {
    val t0 = 0
    val tortoise = 1
    val hare = 2
    assert(states.length >= 3)
    @tailrec
    def goDoubleSpeed(ν: Long): Long = {
      if(eq(states(tortoise), states(hare)))
        ν
      else {
        update(states(tortoise))
        update(states(hare))
        update(states(hare))
        goDoubleSpeed(ν + 1)
      }
    }
    copy(states(tortoise), states(t0))
    update(states(tortoise))
    copy(states(hare), states(tortoise))
    update(states(hare))
    goDoubleSpeed(1)
    @tailrec
    def goNormalSpeed(μ: Long): Long = {
      if(eq(states(tortoise), states(hare)))
        μ
      else {
        update(states(tortoise))
        update(states(hare))
        goNormalSpeed(μ + 1)
      }
    }
    copy(states(tortoise), states(t0))
    val μ = goNormalSpeed(0)
    @tailrec
    def goHare(λ: Long): Long = {
      if(eq(states(tortoise), states(hare)))
        λ
      else {
        update(states(hare))
        goHare(λ + 1)
      }
    }
    copy(states(hare), states(tortoise))
    update(states(hare))
    val λ = goHare(1)
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

  final def unfoldN[A](z: A, n: Long)(f: A => A): A = {
    @annotation.tailrec
    def unfoldN0(z: A, n: Long): A = {
      if(n == 0)
        z
      else
        unfoldN0(f(z), n - 1)
    }
    unfoldN0(z, n)
  }

  final def unfoldWhile[A](z: A)(f: A => A, p: A => Boolean): A = {
    @tailrec
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

  @tailrec
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
  @tailrec
  final def unfoldWithSuffix[S, T](tail: List[T] = Nil)(f: S => Option[(S, T)])(z: S): List[T] = {
    f(z) match {
      case None => tail
      case Some((zz, b)) => unfoldWithSuffix(b :: tail)(f)(zz)
    }
  }

  def generateStreamFrom[S](s0: S)(f: S => S): LazyList[S] =
    s0 #:: generateStreamFrom(f(s0))(f)

  @tailrec
  final def findFixedPoint[A](z: A)(f: A => A): A = {
    val n = f(z)
    if(n == z)
      z
    else
      findFixedPoint(n)(f)
  }

  /** Finds minimum argument for predicate to hold true. */
  @tailrec
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

  /**
    * Searches the position when the function drops from true to false.
    * {{{require(f(low) && !f(high))}}}
    *
    * @param low where to start finding the value boundary
    * @return last position where f is true
    */
  @tailrec
  def findSupremum(f: Int => Boolean, low: Int, high: Int): Int = {
    //    require(f(low) && !f(high))
    val next = low + (high - low) / 2 // this avoids overloading
    if(next == low)
      low
    else if(f(next))
      findSupremum(f, next, high)
    else
      findSupremum(f, low, next)
  }

}
