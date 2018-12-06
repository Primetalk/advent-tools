package org.primetalk.advent2017

import org.primetalk.advent.Geom2dUtils
import org.primetalk.advent2017.Day3._
import org.scalatest.{FlatSpec, Matchers}

class Day3Test extends FlatSpec with Matchers {

  "findUltimatePosition" should "work in simple cases" in {
    findUltimatePosition(1) shouldBe Geom2dUtils.origin
  }

  "fillIn(1)" should "produce result" in {
    fillIn(1) shouldBe 2
    fillIn(5) shouldBe 10
    fillIn(25) shouldBe 26
  }

}
