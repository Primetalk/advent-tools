package org.primetalk.advent2020

import org.primetalk.advent.tools.BaseTest

class Day2015Test extends BaseTest {

  behavior of "Day2015Test"

  it should "nth" in {
    Day2015.nth2(List(1,3,2), 2020) shouldBe 1
//    Given 0,3,6, the 30000000th number spoken is 175594.
//    Given 1,3,2, the 30000000th number spoken is 2578.
//    Given 2,1,3, the 30000000th number spoken is 3544142.
//    Given 1,2,3, the 30000000th number spoken is 261214.
//    Given 2,3,1, the 30000000th number spoken is 6895259.
//    Given 3,2,1, the 30000000th number spoken is 18.
//    Given 3,1,2, the 30000000th number spoken is 362.
  }

}
