package org.primetalk.advent2020

import org.primetalk.advent.tools.BaseTest
import org.primetalk.advent2020.Day2013.{earliestTimestamp, minEst}

class Day2013Test extends BaseTest {

  behavior of "Day2013Test"

  it should "example1" in {
    val earliest = 939
    val ids = List(7,13,59,31,19)
    minEst(earliest, ids) shouldBe 295
  }

  it should "task2 - sample" in {
    Day2013.task2("3,5".split(',').toList) shouldBe 9L
  }

  it should "task2" in {
    Day2013.task2("7,13,x,x,59,x,31,19".split(',').toList) shouldBe 1068781L
  }

  it should "answers" in {
    Day2013.answer1 shouldBe 4315
    Day2013.answer2 shouldBe 556100168221141L
  }
}

