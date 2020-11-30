package org.primetalk.advent2019

class Day3Test extends BaseTest {

  "min cross distance 1" should "be 6"  in {
    val w1 = "R8,U5,L5,D3"
    val w2 = "U7,R6,D4,L4"
    val d = 6
    Day3.minCrossDistance(w1, w2) shouldBe d
  }

  "min cross distance 2" should "be 159"  in {
    val w1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    val w2 = "U62,R66,U55,R34,D71,R55,D58,R83"
    val d = 159
    Day3.minCrossDistance(w1, w2) shouldBe d
  }

  "min cross distance 3" should "be 135"  in {
    val w1 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    val w2 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    val d = 135
    Day3.minCrossDistance(w1, w2) shouldBe d
  }

  "min signal delay 1" should "be 30"  in {
    val w1 = "R8,U5,L5,D3"
    val w2 = "U7,R6,D4,L4"
    val d = 30
    Day3.minSignalDelay(w1, w2) shouldBe d
  }
  "min signal delay 2" should "be 610"  in {
    val w1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    val w2 = "U62,R66,U55,R34,D71,R55,D58,R83"
    val d = 610
    Day3.minSignalDelay(w1, w2) shouldBe d
  }


}
