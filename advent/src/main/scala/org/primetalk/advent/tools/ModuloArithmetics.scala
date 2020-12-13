package org.primetalk.advent.tools

object ModuloArithmetics {
  def modInverse(a: BigInt, modulo: BigInt): BigInt = {
    try {
      a.modInverse(modulo)
    } catch {
      case e: ArithmeticException =>
        throw new IllegalArgumentException(s"Couldn't invert $a in modulo $modulo", e)
    }
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
        val `M_i^-1` = modInverse(M_i, a_i)
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

}
