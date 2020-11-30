package org.primetalk.advent2019

class Day7Test extends BaseTest {

  behavior of "Day7Test"

  it should "findLargestPermutation2" in {
    val input = Seq(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
      27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
    Day7.findLargestPermutation2(input) shouldBe 139629729
  }

}
