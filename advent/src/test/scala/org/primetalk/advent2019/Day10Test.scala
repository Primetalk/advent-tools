package org.primetalk.advent2019

class Day10Test extends BaseTest {

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
