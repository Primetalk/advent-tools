package org.primetalk.advent3.tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NumberSequenceUtilsTest extends UnitSpec {

  behavior of "SequenceUtilsTest"

  def f(i: Int): Int = (i + 3) % 10

  it should "floyd" in {
    val (s, l) = NumberSequenceUtils.floydInt(11)()(f)
    s shouldBe 1
    l shouldBe 10
  }

  it should "floydMutable" in {
    class I {
      var i: Int = 11
      def step(): Unit = { i = f(i)}
    }
    val arr = Array.fill[I](3)(new I)
    val (s, l) = NumberSequenceUtils.floydMutable[I](arr)(_.i == _.i, (a,b) => a.i = b.i)(_.step())
    s shouldBe 1
    l shouldBe 10
  }
  it should "unfoldN" in {
    NumberSequenceUtils.unfoldN(f)(0,2) shouldBe 6
  }

  it should "find infinum" in {
    NumberSequenceUtils.binFindInfinum(_ > 10)(0, 100) shouldBe 11
  }
  it should "find supremum" in {
    NumberSequenceUtils.binFindSupremum(_ < 10)(0, 100) shouldBe 9
  }
}
