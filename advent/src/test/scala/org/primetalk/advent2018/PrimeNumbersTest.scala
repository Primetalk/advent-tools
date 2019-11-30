package org.primetalk.advent2018

import org.primetalk.advent.tools.PrimeNumbers._

class PrimeNumbersTest extends BaseTest {

  behavior of "PrimeNumbersTest"

  it should "allDivisors" in {
    allDivisors(20) shouldBe Seq(1, 2, 4, 5, 10, 20)
  }

  it should "factoriseToPrimes" in {
    factoriseToPrimes(10) shouldBe Seq(2, 5)
    factoriseToPrimes(20) shouldBe Seq(2, 2, 5)
  }

  it should "factoriseToFactorPowers" in {
    factoriseToFactorPowers(10) shouldBe Seq(Factor(2, 1), Factor(5, 1))
    factoriseToFactorPowers(20) shouldBe Seq(Factor(2, 2), Factor(5, 1))
  }

  it should "primeNumbers" in {
    primeNumbers(19) shouldBe Seq(2, 3, 5, 7, 11, 13, 17, 19)
  }

  it should "primeDivisors" in {
    primeDivisors(19) shouldBe Seq(19)
    primeDivisors(38) shouldBe Seq(2, 19)
    primeDivisors(20) shouldBe Seq(2, 5)
  }

}
