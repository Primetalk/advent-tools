package org.primetalk.advent.tools

import scala.annotation.tailrec

object PrimeNumbers {

  val bigPrimeNumber1: Int = 1_000_000_000 + 7
  val bigPrimeNumber2: Int = 1_000_000_000 + 9

  /** Finds prime numbers up to m. */
  def primeNumbers(m: Int): Seq[Int] = {
    val offset = 2
    val sieve = (offset to m).toArray[Int]
    @scala.annotation.tailrec
    def strikeOut(p: Int, step: Int): Unit = {
      if(p <= m) {
        sieve(p - offset) = 0
        strikeOut(p + step, step)
      }
    }
    @scala.annotation.tailrec
    def go(i: Int): Unit = {
      if(i < m) {
        if(sieve(i - offset) != 0){
          strikeOut(i * 2, i)
        }
        go(i + 1)
      }
    }
    go(offset)
    sieve.toSeq.filterNot(_ == 0)
  }

  def factoriseToPrimes(n: Int): List[Int] = {
    val max = math.sqrt(n + 1).toInt
    val primes = primeNumbers(max)
    @scala.annotation.tailrec
    def go(divs: List[Int], m: Int, res: List[Int]): List[Int] = divs match {
      case Nil if m == 1 => res
      case Nil => m :: res
      case h :: _ if m % h == 0 =>
        go(divs, m / h, h :: res)
      case _ :: t =>
        go(t, m, res)
    }
    go(primes.toList, n, Nil).sorted
  }

  def primeDivisors(n: Int): Seq[Int] =
    factoriseToPrimes(n).distinct

  /** Relatively slow int power.
    * Doesn't use logarithm.
    */
  @scala.annotation.tailrec
  def intPow(n: Int, power: Int, mul: Int = 1): Int = {
    if(power == 0)
      mul
    else
      intPow(n, power - 1, mul * n)
  }

  @scala.annotation.tailrec
  def longPow(n: Long, power: Int, mul: Long = 1): Long = {
    if(power == 0)
      mul
    else
      longPow(n, power - 1, mul * n)
  }

  case class Factor(prime: Int, power: Int) {
    def value: Int = intPow(prime, power)
    /** \phi(prime&#94;power) = prime&#94;power - prime&#94;(power-1) */
    def euler: Int = intPow(prime, power) - intPow(prime, power - 1)
    def toSeq: Seq[Int] = Seq.fill(power)(prime)
  }

  def factoriseToFactorPowers(n: Int): Seq[Factor] = {
    factoriseToPrimes(n)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .map{ case (i,p) => Factor(i,p) }
      .toSeq
  }

  def multiply(factors: Seq[Factor]): Int = {
    factors.foldLeft(1){ case (p, f) => p * f.value}
  }

  def allDivisors(n: Int): Seq[Int] = {
    val allFactors = factoriseToFactorPowers(n).flatMap(_.toSeq)
    (1 +: allFactors.indices.flatMap(i => allFactors.combinations(i + 1).map(_.product))).distinct.sorted
  }

  /**
    * Solves the equation
    *  i * x + divisor * y = d
    */
  @scala.annotation.tailrec
  def greatestCommonDivisorInt(i: Int, divisor: Int): Int =
    if(i >= divisor) {
      val nextDivisor = i % divisor
      if(nextDivisor == 0)
        divisor
      else
        greatestCommonDivisorInt(divisor, nextDivisor)
    } else greatestCommonDivisorInt(divisor,i)

  @scala.annotation.tailrec
  def greatestCommonDivisorLong(i: Long, divisor: Long): Long =
    if(i >= divisor) {
      val nextDivisor = i % divisor
      if(nextDivisor == 0)
        divisor
      else
        greatestCommonDivisorLong(divisor, nextDivisor)
    } else greatestCommonDivisorLong(divisor,i)

  @scala.annotation.tailrec
  def greatestCommonDivisor[T: Integral](i: T, divisor: T): T = {
    val num = Integral[T]
    import num._
    if (i >= divisor) {
      val nextDivisor = i % divisor
      if (nextDivisor == zero)
        divisor
      else
        greatestCommonDivisor(divisor, nextDivisor)
    } else greatestCommonDivisor(divisor, i)
  }

//    @scala.annotation.tailrec mod inverse
//    def loop(t: BigInt = 0, newt: BigInt = 1, r: BigInt = n, newr: BigInt = a): BigInt = {
//      if(newr == 0){
//        if(r > 1)
//          throw new IllegalArgumentException(s"${a} is not invertible module $n")
//        else if(t < 0)
//          t + n
//        else
//          t
//      } else {
//        val quotient = r / newr
//        loop(newt, t - quotient * newt, newr, r - quotient * newr)
//      }
//    }
//    loop()

//  /** Find 1/n (% mod). GCD(n, mod) == 1 */
//  def inverse(n: BigInt, mod: BigInt): Long = {
//    (mod + 1) / n
//    @scala.annotation.tailrec
//    def loop(k: Long): Long = {
//      val candidate = (mod*k + 1) / n
//      if(candidate*n % mod == BigInt(1) )
//        candidate.toLong
//      else {
//        println(s"looping $k")
//        loop(k + 1)
//      }
//
//    }
//    loop(1)
//  }
  def leastCommonMultipleInt(a: Int, b: Int): Long =
    a.toLong * b / greatestCommonDivisorInt(a, b)

  def leastCommonMultipleLong(a: Long, b: Long): Long =
    a.toLong * b / greatestCommonDivisorLong(a, b)

  def leastCommonMultiple[T: Integral](a: T, b: T): T = {
    val num = Integral[T]
    import num._

    a * b / greatestCommonDivisor(a, b)
  }

  def leastCommonMultiple3Long(a: Long, b: Long, c: Long): Long =
    leastCommonMultipleLong(leastCommonMultipleLong(a,b),c)

  def leastCommonMultipleN[T: Integral](s: Seq[T]): T =
    s.reduce((a,b) => leastCommonMultiple(a,b))

  type FactorizedNumber = List[Factor]

  @tailrec
  def factorizedNumberValue(n: FactorizedNumber, result: Long = 1L): Long = n match {
    case Nil =>
      result
    case ::(head, tail) =>
      factorizedNumberValue(tail, head.value * result)
  }
  /** Euler function can be easily calculated for
    * factorized numbers.
    * NB! factors should be such that: GCD(p_i, p_j) = 1
    * there exists similar formula for an alternative case where GCD = d
    * in this case
    * phi(m*n) = phi(m) * phi(n) * d/phi(d)
    */
  def euler(n: FactorizedNumber): Long =
    n.map(_.euler.toLong).product
}
