package org.primetalk.advent2019

import org.scalatest.{FlatSpec, Matchers}

class Day10Test extends FlatSpec with Matchers {

  behavior of "Day10Test"

  "test1" should "shouldBe ((5,8), 33)" in {
    val test1 =
      """......#.#.
        |#..#.#....
        |..#######.
        |.#.#.###..
        |.#..#.....
        |..#....#.#
        |#..#....#.
        |.##.#..###
        |##...#..#.
        |.#....####
        |""".stripMargin.split('\n').toSeq
    Day10.stationLocation(test1) shouldBe ((5,8), 33)
  }

}
