package org.primetalk.advent

import org.scalatest.{FlatSpec, Matchers}
import Day1._

class Day1Test extends FlatSpec with Matchers {

  "+1, +1, +1" should "result in  3" in {
    task1("+1\n+1\n+1") shouldBe 3
  }

  "other examples" should "match" in {
    task1("+1\n+1\n-2") shouldBe 0
    task1("-1\n-2\n-3") shouldBe -6
  }

}