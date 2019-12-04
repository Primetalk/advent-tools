package org.primetalk.advent2019

import org.scalatest.{FlatSpec, Matchers}

class Day4Test extends FlatSpec with Matchers {

  behavior of "Day4"

  "isAGoodPassword" should "have doubles" in {
    Day4.isAGoodPassword(111111) shouldBe true
  }

  "isAGoodPassword" should "increase" in {
    Day4.isAGoodPassword(223450) shouldBe false
  }

  "isAGoodPassword" should "have doubles neg" in {
    Day4.isAGoodPassword(123789) shouldBe false
  }

  "digits10" should "work correctly" in {
    Day4.digits10(123789) shouldBe List(1,2,3,7,8,9)
  }

  "hasPair" should "work on examples" in {
    Day4.hasPair(List(1,1,1,1,2,2)) shouldBe true
    Day4.hasPair(List(1,2,3,4,4,4)) shouldBe false
  }
}
