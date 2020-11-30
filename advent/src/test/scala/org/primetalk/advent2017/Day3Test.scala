package org.primetalk.advent2017

import org.primetalk.advent.tools.Geom2dUtils
import org.primetalk.advent2017.Day3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3Test extends AnyFlatSpec with Matchers {

  "findUltimatePosition" should "work in simple cases" in {
    findUltimatePosition(1) shouldBe Geom2dUtils.origin
  }

//  "fillIn(1)" should "produce result" in {
//    fillIn(1) shouldBe 2
//    fillIn(5) shouldBe 10
//    fillIn(25) shouldBe 26
//  }

}
