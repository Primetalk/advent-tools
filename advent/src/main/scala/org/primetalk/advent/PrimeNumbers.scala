package org.primetalk.advent

object PrimeNumbers {

  /** Finds prime numbers up to m. */
  def primeNumbers(m: Int): Seq[Int] = {
    val offset = 2
    val sieve = (offset to m).toArray[Int]
    def strikeOut(p: Int, step: Int): Unit = {
      if(p <= m) {
        sieve(p - offset) = 0
        strikeOut(p + step, step)
      }
    }
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

  def factoriseToPrimes(n: Int): Seq[Int] = {
    val max = math.sqrt(n + 1).toInt
    val primes = primeNumbers(max)
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

  def intPow(n: Int, power: Int, mul: Int = 0): Int = {
    if(power == 0)
      mul
    else
      intPow(n, power - 1, mul * n)
  }

  case class Factor(n: Int, power: Int) {
    def value: Int = intPow(n, power)
    def toSeq: Seq[Int] = Seq.fill(power)(n)
  }

  def factoriseToFactorPowers(n: Int): Seq[Factor] = {
    factoriseToPrimes(n).groupBy(identity)
      .mapValues(_.size)
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
}
