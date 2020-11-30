package org.primetalk.advent.tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SequenceUtilsTest extends AnyFlatSpec with Matchers {

  behavior of "SequenceUtilsTest"

  def f(i: Int): Int = (i + 3) % 10
  it should "floyd" in {
    val (s, l) = SequenceUtils.floydInt(11)()(f)
    s shouldBe 1
    l shouldBe 10
  }

  it should "floydMutable" in {
    class I {
      var i: Int = 11
      def step(): Unit = { i = f(i)}
    }
    val arr = Array.fill[I](3)(new I)
    val (s, l) = SequenceUtils.floydMutable[I](arr)(_.i == _.i, (a,b) => a.i = b.i)(_.step())
    s shouldBe 1
    l shouldBe 10
  }

}
