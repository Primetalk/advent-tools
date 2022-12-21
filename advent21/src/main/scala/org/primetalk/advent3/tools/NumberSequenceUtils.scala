package org.primetalk.advent3.tools

import scala.annotation.tailrec

object NumberSequenceUtils:

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
  final def floydInt[T](t0: T)(eq: (T, T) => Boolean = (a: T, b: T) => a == b)(f: SequenceGen[T] ): (Int, Int) = {

    @tailrec
    def goDoubleSpeed(tortoise: T, hare: T, ν: Int): (T, T, Int) =
      if(eq(tortoise, hare))
        (tortoise, hare, ν)
      else
        goDoubleSpeed(f(tortoise), f(f(hare)), ν + 1)

    val t1 = f(t0)
    val (_, hare, _) = goDoubleSpeed(t1, f(t1), 1)
    @tailrec
    def goNormalSpeed(tortoise: T, hare: T, μ: Int): (T, T, Int) =
      if(eq(tortoise, hare))
        (tortoise, hare, μ)
      else
        goNormalSpeed(f(tortoise), f(hare), μ + 1)

    val (tortoise2, _, μ) = goNormalSpeed(t0, hare, 0)
    @tailrec
    def goHare(tortoise: T, hare: T, λ: Int): Int =
      if(eq(tortoise, hare))
        λ
      else
        goHare(tortoise, f(hare), λ + 1)

    val λ = goHare(tortoise2, f(tortoise2), 1)
    (μ, λ)
  }

  final def floyd[T](t0: T)(eq: (T, T) => Boolean = (a: T, b: T) => a == b)(f: SequenceGen[T] ): (Long, Long) =

    @tailrec
    def goDoubleSpeed(tortoise: T, hare: T, ν: Long): (T, T, Long) =
      if(eq(tortoise, hare))
        (tortoise, hare, ν)
      else
        goDoubleSpeed(f(tortoise), f(f(hare)), ν + 1)
    
    val t1 = f(t0)
    val (_, hare, _) = goDoubleSpeed(t1, f(t1), 1)

    @tailrec
    def goNormalSpeed(tortoise: T, hare: T, μ: Long): (T, T, Long) =
      if(eq(tortoise, hare))
        (tortoise, hare, μ)
      else
        goNormalSpeed(f(tortoise), f(hare), μ + 1)
    
    val (tortoise2, _, μ) = goNormalSpeed(t0, hare, 0)

    @tailrec
    def goHare(tortoise: T, hare: T, λ: Long): Long =
      if(eq(tortoise, hare))
        λ
      else
        goHare(tortoise, f(hare), λ + 1)
    
    val λ = goHare(tortoise2, f(tortoise2), 1)
    (μ, λ)


  // states should contain 3 elements.
  // [0] - initial state
  // [1] - tortoise - any value
  // [2] - hare - any value
  // @returns (μ, λ) - (shift, period)
  //    NB, shift might not be enough when there are some parallel processes happen.
  // one might need to add period to it to come to a stable place
  @inline
  def floydMutable[T](states: Array[T])(eq: (T, T) => Boolean = (a: T, b: T) => a == b, copy: (T, T) => Unit)(update: T => Unit): (Long, Long) =
    val t0 = 0
    val tortoise = 1
    val hare = 2
    assert(states.length >= 3)
    @tailrec
    def goDoubleSpeed(ν: Long): Long =
      if eq(states(tortoise), states(hare)) then
        ν
      else
        update(states(tortoise))
        update(states(hare))
        update(states(hare))
        goDoubleSpeed(ν + 1)

    copy(states(tortoise), states(t0))
    update(states(tortoise))
    copy(states(hare), states(tortoise))
    update(states(hare))
    goDoubleSpeed(1)
    @tailrec
    def goNormalSpeed(μ: Long): Long =
      if eq(states(tortoise), states(hare)) then
        μ
      else
        update(states(tortoise))
        update(states(hare))
        goNormalSpeed(μ + 1)

    copy(states(tortoise), states(t0))
    val μ = goNormalSpeed(0)
    @tailrec
    def goHare(λ: Long): Long =
      if eq(states(tortoise), states(hare)) then
        λ
      else
        update(states(hare))
        goHare(λ + 1)

    copy(states(hare), states(tortoise))
    update(states(hare))
    val λ = goHare(1)
    (μ, λ)
  /** Returns the last valid element in the sequence. */ 
  inline final def unfold[A](gen: FiniteSequenceGen[A]): (z: A) => A =
    @annotation.tailrec
    def unfold0(z: A): A =
      gen(z) match
        case None => z
        case Some(zz) => unfold0(zz)
    
    unfold0
  
  /** Finds Nth element of the sequence. 
   * if n == 0 returns the provided z, otherwise - f^n(z)
   */
  inline final def unfoldN[A](f: A => A): (z: A, n: Long) => A =
    @annotation.tailrec
    def unfoldN0(z: A, n: Long): A =
      if(n == 0)
        z
      else
        unfoldN0(f(z), n - 1)
    
    unfoldN0
  
  /** Unfolds the given function while predicate is true. 
   * returns the first element when it becomes false.
   */ 
  final def unfoldWhile[A](f: A => A, p: A => Boolean): (z: A) => A =
    @tailrec
    def unfoldWhile0(z: A): A =
      if p(z) then
        val next = f(z)
        if next == z then throw IllegalStateException("Stuck at " + z)
        unfoldWhile0(next)
      else
        z
    
    unfoldWhile0
  

  
  final def unfoldUntil[A](f: A => A)(p: A => Boolean): (z: A) => A = 
    @tailrec
    def unfoldUntil0(z: A): A =
      if p(z) then
        z
      else 
        val next = f(z)
        if next == z then throw IllegalStateException("Stuck at " + z)
        unfoldUntil0(next)
      
    unfoldUntil0

  final def countUntil[A](f: A => A)(p: A => Boolean): (z: A) => (A, Int) = 
    (z: A) => unfoldUntil[(A, Int)]( ac => ({/*print(s"${ac._2}:");*/f(ac._1)}, ac._2 + 1))(ac => p(ac._1))((z, 0))

  /** Starts with initial state `S`, then applies `f` continuously.
    * Each step adds something to output and updates state.
    * When `f` yields `None`, stops.
    *
    * O(N)
    */
  final def unfoldWithSuffix[S, T](f: S => Option[(S, T)]): (tail: List[T], z: S) => List[T] =
    @tailrec
    def unfoldWithSuffix0(tail: List[T] = Nil, z: S): List[T] =
      f(z) match
        case None => tail
        case Some((zz, b)) => unfoldWithSuffix0(b :: tail, zz)

    unfoldWithSuffix0

  def generateStreamFrom[S](s0: S)(f: S => S): LazyList[S] =
    s0 #:: generateStreamFrom(f(s0))(f)

  @tailrec
  final def fixedPoint[A](z: A)(f: A => A): A =
    val n = f(z)
    if n == z then
      z
    else
      fixedPoint(n)(f)
  
  @tailrec
  final def countUntilFixedPoint[A](z: A, cnt: Long = 0)(f: A => A, eq: (A, A) => Boolean ): (A, Long) =
    val n = f(z)
    if cnt % 1000 == 0 then println(cnt)
    if eq(n, z) then
      (z, cnt)
    else
      countUntilFixedPoint(n, cnt + 1)(f, eq)
  

  /** Finds minimum argument for predicate to hold true. */
  @tailrec
  def binFindInfinumOld(p: Predicate[Long])(guessBelow: Long, guessAbove: Long): Long =
    require(!p(guessBelow) && p(guessAbove))
    if guessAbove == guessBelow + 1 then
      guessAbove
    else 
      val next = (guessAbove + guessBelow) / 2
      if p(next) then
        binFindInfinumOld(p)(guessBelow, next)
      else
        binFindInfinumOld(p)(next, guessAbove)
  /**
    * Searches the minimum position when the function drops from true to false.
    * {{{require(!f(low) && f(high))}}}
    *
    * @param low where to start finding the value boundary
    * @return lowest position where f is true
    */
  def binFindInfinum(p: Predicate[Long])(low: Long, high: Long): Long =
    require(!p(low) && p(high))
    @tailrec
    def binFindInfinum0(low: Long, high: Long): Long =
      val next = low + (high - low) / 2 // this avoids overloading
      if next == low then
        high
      else if p(next) then
        binFindInfinum0(low, next)
      else
        binFindInfinum0(next, high)
    binFindInfinum0(low, high)
  /**
    * Searches the position when the function drops from true to false.
    * {{{require(f(low) && !f(high))}}}
    *
    * @param low where to start finding the value boundary
    * @return last position where f is true
    */
  def binFindSupremum(p: Predicate[Long])(low: Long, high: Long): Long =
    require(p(low) && !p(high))
    @tailrec
    def binFindSupremum0(low: Long, high: Long): Long =
      val next = low + (high - low) / 2 // this avoids overloading
      if next == low then
        low
      else if p(next) then
        binFindSupremum0(next, high)
      else
        binFindSupremum0(low, next)
    binFindSupremum0(low, high)
  
  /** Function should have a single strong minimum in the given range.
   * The search maintains three points with values. Each turn it selects another point in the middle 
   * of the larger interval and decides which 3 points to keep.
   * Search continues until there are 3 consequtive points - x, x+1,x+2. 
   * And then selects the lowest of them.
   * Function is evaluated at most once at points.
  */
  def findSingleMinumum[T: Ordering](f: Int => T)(lower: Int, upper: Int): (Int, T) = 
    val ord = summon[Ordering[T]]
    import ord.mkOrderingOps
    case class PosValue(pos: Int, value: T)
    def fpv(pos: Int): PosValue = 
      PosValue(pos, f(pos))
    def findSingleMinumum0(l: PosValue, m: PosValue, u: PosValue): PosValue = 
      if u.pos <= l.pos + 2 then
        Seq(l, m, u).minBy(_.value)
      else 
        val (m1, m2) = 
          if u.pos - m.pos > m.pos - l.pos then
            (m, fpv(m.pos + (u.pos - m.pos)/2))
          else
            (fpv(l.pos + (m.pos - l.pos)/2), m)

        if m1.value < m2.value then 
          findSingleMinumum0(l, m1, m2)
        else
          findSingleMinumum0(m1, m2, u)

    val fl = fpv(lower) // we could also start at mid, but that would duplicate mid calculation code
    val PosValue(p, fp) = findSingleMinumum0(fl, fl, fpv(upper))
    (p, fp)
  