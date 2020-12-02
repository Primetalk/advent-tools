package org.primetalk.advent2019

class Day16Test extends BaseTest {

  behavior of "Day16Test"

  "pattern" should "be known" in {
    Day16.pattern(0)(0) shouldBe 0
    Day16.pattern(0)(1) shouldBe 1
    Day16.pattern(0)(2) shouldBe 0
    Day16.pattern(0)(3) shouldBe -1
  }
  it should "phase" in {
    Day16.phase(Day16.parse("12345678")) shouldBe Day16.parse("48226158")
//    Day16.phase(Day16.parse("48226158")) shouldBe Day16.parse("48226158")
  }

  "answer1" should "be known" in {
    Day16.answer1 shouldBe "63794407"
  }

  "answer2" should "be known" in {
    Day16.answer2 shouldBe "77247538"
  }

}
