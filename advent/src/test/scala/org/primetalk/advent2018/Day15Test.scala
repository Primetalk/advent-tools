package org.primetalk.advent2018

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
//  it should "playGame 2" in {
//    import Day15._
//    val game1 =
//      """#######
//        |#.G...#
//        |#...EG#
//        |#.#.#G#
//        |#..G#E#
//        |#.....#
//        |#######
//        |""".stripMargin
//
//    playGame(game1) shouldBe 27730
//  }


  /*
#######       #######
#G..#E#       #...#E#   E(200)
#E#E.E#       #E#...#   E(197)
#G.##.#  -->  #.E##.#   E(185)
#...#E#       #E..#E#   E(200), E(200)
#...E.#       #.....#
#######       #######

Combat ends after 37 full rounds
Elves win with 982 total hit points left
Outcome: 37 * 982 = 36334
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

  /*
#######       #######
#E..EG#       #.E.E.#   E(164), E(197)
#.#G.E#       #.#E..#   E(200)
#E.##E#  -->  #E.##.#   E(98)
#G..#.#       #.E.#.#   E(200)
#..E#.#       #...#.#
#######       #######

Combat ends after 46 full rounds
Elves win with 859 total hit points left
Outcome: 46 * 859 = 39514
   */
//  it should "playGame 4" in {
//    import Day15._
//    val game1 =
//      """#######
//        |#E..EG#
//        |#.#G.E#
//        |#E.##E#
//        |#G..#.#
//        |#..E#.#
//        |#######
//        |""".stripMargin
//
//    playGame(game1) shouldBe 36334
//  }
  /*
#######       #######
#E.G#.#       #G.G#.#   G(200), G(98)
#.#G..#       #.#G..#   G(200)
#G.#.G#  -->  #..#..#
#G..#.#       #...#G#   G(95)
#...E.#       #...G.#   G(200)
#######       #######

Combat ends after 35 full rounds
Goblins win with 793 total hit points left
Outcome: 35 * 793 = 27755
   */
  it should "playGame 5" in {
    import Day15._
    val game1 =
      """#######
        |#E.G#.#
        |#.#G..#
        |#G.#.G#
        |#G..#.#
        |#...E.#
        |#######
        |""".stripMargin

    playGame(game1) shouldBe 27755
  }
}
