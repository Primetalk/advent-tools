package org.primetalk.advent3.tools

import scala.math.BigInt

object Combinations:

  def factorial(n: Int, accum: BigInt = 1): BigInt =
    if n <= 0 then
      accum
    else
      factorial(n - 1, accum * n)

  def binomialCoefficient(n: Int, k: Int): BigInt =
    factorial(n) / factorial(k) / factorial(n - k)

  /** C^n_k - binomial coefficient. */
  inline def combinationsCount(n: Int, k: Int): BigInt =
    binomialCoefficient(n, k)

end Combinations
