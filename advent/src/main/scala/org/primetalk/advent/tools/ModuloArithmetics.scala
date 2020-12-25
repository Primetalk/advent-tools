package org.primetalk.advent.tools

import scala.annotation.tailrec

object ModuloArithmetics {
  val one = BigInt(1)

  case class ModuloField(modulo: BigInt) {
    def apply(a: BigInt): BigInt =
      a % modulo

    def add(a: BigInt, b: BigInt): BigInt =
      (a + b) % modulo

    def mul(a: BigInt, b: BigInt): BigInt =
      (a * b) % modulo

    def div(a: BigInt, b: BigInt): BigInt =
      mul(a, inverse(b))

    def sub(a: BigInt, b: BigInt): BigInt =
      (a - b + modulo) % modulo

    def neg(b: BigInt): BigInt =
      (modulo - b) % modulo

    def inverse(a: BigInt): BigInt =
      try {
        a.modInverse(modulo)
      } catch {
        case e: ArithmeticException =>
          throw new IllegalArgumentException(s"Couldn't invert $a in modulo $modulo", e)
      }
    @tailrec
    final def linearModPower(a: Long, n: BigInt, res: BigInt = one): BigInt =
      if(n == 0)
        res
      else
        linearModPower(a, (res * a) % modulo, n - 1)

    def linearLogarithm(res: BigInt, base: BigInt): Long = {
      @tailrec
      def loop(res: BigInt, invBase: BigInt, count: Long = 0L): Long =
        if(res == one)
          count
        else
          loop(res * invBase % modulo, invBase, count + 1)
      loop(res, inverse(base))
    }

    def power(base: BigInt, power: BigInt): BigInt =
      base.modPow(power, modulo)
  }

  implicit class ModuloOps(a: BigInt)(implicit field: ModuloField) {
    def *%(b: BigInt): BigInt = field.mul(a, b)
    def +%(b: BigInt): BigInt = field.add(a, b)
    def -%(b: BigInt): BigInt = field.sub(a, b)
    def ^%(b: BigInt): BigInt = field.power(a, b)
    def log_%(base: BigInt): BigInt = field.linearLogarithm(a, base)
    def inv_% : BigInt = field.inverse(a)
    def neg_% : BigInt = field.neg(a)
  }

  case class RemainderAndModulo(r: BigInt, m: BigInt)

  /** Solves the following system of equations:
    * {{{
    *   { x ≡ r_i (mod a_i)
    *   where
    *         GCD(a_i, a_j) = 1
    *         0 &lt;= r_i &lt; a_i
    * }}}
    *
    * Solution:
    * {{{
    * M = ∏_i a_i
    * M_i = M/a_i
    * M_i&#94;-1 ≡ 1/M_i (mod a_i)
    * x = ∑_i r_i * M_i * M_i&#94;-1
    * }}}
    */
  def chineseReminderTheorem(equations: List[RemainderAndModulo]): BigInt = {
    val a = equations.map(_.m)
    val M = PrimeNumbers.leastCommonMultipleN(a)
    equations.map{
      case RemainderAndModulo(r_i, a_i) =>
        val M_i = M / a_i
        val `M_i^-1` = ModuloField(a_i).inverse(M_i)
        r_i * M_i * `M_i^-1` % M
    }
      .sum % M
  }

  // fast binary power algorithm
  def modPower(a: BigInt, n: BigInt, mod: BigInt): BigInt = {
    a.modPow(n, mod)
    //    @scala.annotation.tailrec
    //    def loop(current: BigInt, n: BigInt, mul: BigInt): BigInt = {
    //      if(n == 1) {
    //        current * mul % mod
    //      } else {
    //        val nextN = n / 2
    //        val p2 = current * current % mod
    //        loop(p2, nextN,
    //          if(n % 2 == 1)
    //            current * mul  % mod
    //          else
    //            mul
    //        )
    //      }
    //    }
    //    loop(a, n, 1)
  }


//  /** Find 1/n (% mod).
//    * NB! GCD(n, mod) == 1
//    *
//    * Otherwise use BigInt.modInverse
//    */
//  def inverse(n: Long, mod: BigInt): Long = {
//
//    modPower(n, mod - 2, mod).toLong // Fermat's little theorem
//    //PrimeNumbers.modInverse(n, mod)
//  }
}
