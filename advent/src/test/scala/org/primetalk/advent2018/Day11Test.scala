package org.primetalk.advent2018

import org.scalatest.{FlatSpec, Matchers}
import Day11._

class Day11Test extends FlatSpec with Matchers {

  behavior of "Day11Test"

  it should "calculatePowerLevel" in {
    calculatePowerLevel(8 )((  3,  5)) shouldBe 4
    calculatePowerLevel(57)((122, 79)) shouldBe -5
    calculatePowerLevel(39)((217,196)) shouldBe 0
    calculatePowerLevel(71)((101,153)) shouldBe 4
  }

}
