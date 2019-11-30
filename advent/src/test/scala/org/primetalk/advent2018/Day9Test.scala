package org.primetalk.advent2018

import org.scalatest.{FlatSpec, Matchers}
import Day9._

class Day9Test extends FlatSpec with Matchers {

  behavior of "Day9Test"

  /*

  9 25
      10 players; last marble is worth 1618 points: high score is 8317
      13 players; last marble is worth 7999 points: high score is 146373
      17 players; last marble is worth 1104 points: high score is 2764
      21 players; last marble is worth 6111 points: high score is 54718
      30 players; last marble is worth 5807 points: high score is 37305


   */
  it should "work in example cases" in {
    score(runUntilEnd(9, 25)) shouldBe 32
    score(runUntilEnd(10, 1618)) shouldBe 8317
    score(runUntilEnd(13, 7999)) shouldBe 146373
    score(runUntilEnd(17, 1104)) shouldBe 2764
    score(runUntilEnd(21, 6111)) shouldBe 54718
    score(runUntilEnd(30, 5807)) shouldBe 37305
  }

}
