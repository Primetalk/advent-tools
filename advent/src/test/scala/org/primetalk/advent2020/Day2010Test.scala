package org.primetalk.advent2020

import org.primetalk.advent.tools.BaseTest

class Day2010Test extends BaseTest {

  behavior of "Day2010Test"

//  it should "answers" in {
//    Day2010.answer1 shouldBe 0
//    Day2010.answer2 shouldBe 0
//  }

  val test1 = List(16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4)

  val test2 = List(28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3)
  it should "pass 1-st test" in {
    Day2010.findConnection(test1) shouldBe 35
  }
  "count combinations" should "pass test1" in {
    Day2010.countCombinationsMain(Day2010.lineUp(test1)) shouldBe 8L
  }
  "count combinations" should "pass test2" in {
    Day2010.countCombinationsMain(Day2010.lineUp(test2)) shouldBe 19208L
  }
}
