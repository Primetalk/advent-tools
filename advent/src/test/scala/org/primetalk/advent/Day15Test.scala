package org.primetalk.advent

class Day15Test extends BaseTest {

  behavior of "Day15Test"

  it should "playGame" in {
    import Day15._
    val game1 =
      """#########
        |#G..G..G#
        |#.......#
        |#.......#
        |#G..E..G#
        |#.......#
        |#.......#
        |#G..G..G#
        |#########
        |""".stripMargin

    playGame(game1) //shouldBe greater
//
//    val round1 = """#########
//                   |#.G...G.#
//                   |#...G...#
//                   |#...E..G#
//                   |#.G.....#
//                   |#.......#
//                   |#G..G..G#
//                   |#.......#
//                   |#########
//                   |""".stripMargin
  }
// 27589 was not equal to 27730
  it should "playGame 2" in {
    import Day15._
    val game1 =
      """#######
        |#.G...#
        |#...EG#
        |#.#.#G#
        |#..G#E#
        |#.....#
        |#######
        |""".stripMargin

    playGame(game1) shouldBe 27730
  }


  /*
#######       #######
#G..#E#       #...#E#   E(200)
#E#E.E#       #E#...#   E(197)
#G.##.#  -->  #.E##.#   E(185)
#...#E#       #E..#E#   E(200), E(200)
#...E.#       #.....#
#######       #######
   */
  it should "playGame 3" in {
    import Day15._
    val game1 =
      """#######
        |#G..#E#
        |#E#E.E#
        |#G.##.#
        |#...#E#
        |#...E.#
        |#######
        |""".stripMargin

    playGame(game1) shouldBe 36334
  }


}
