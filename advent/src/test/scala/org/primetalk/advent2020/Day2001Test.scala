package org.primetalk.advent2020

class Day2001Test extends BaseTest {

  import Day2001._

  behavior of "Day2001Test"

  it should "find pair" in {
    answer1 shouldBe List(1014171)
  }

  it should "find triple" in {
    answer2.distinct shouldBe List(46584630)
  }

}
