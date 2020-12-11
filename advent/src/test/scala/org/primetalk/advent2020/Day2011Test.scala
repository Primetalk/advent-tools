package org.primetalk.advent2020

import org.primetalk.advent.tools.{BaseTest, Display}
import org.primetalk.advent2020.Day2011.{findStable, step2}

class Day2011Test extends BaseTest {

  behavior of "Day2011Test"

  val test: String =
    """L.LL.LL.LL
      |LLLLLLL.LL
      |L.L.L..L..
      |LLLL.LL.LL
      |L.LL.LL.LL
      |L.LLLLL.LL
      |..L.L.....
      |LLLLLLLLLL
      |L.LLLLLL.L
      |L.LLLLL.LL
      |""".stripMargin
  val testDisplay1: Display[Char] = Display.readCharDisplay(test.split('\n').toSeq, '.')
  it should "step" in {
//    val d1 = Day2011.step(testDisplay1)
////    println(d1.showDisplay()(_.toString))
//    val d2 = Day2011.step(d1)
////    println(d2.showDisplay()(_.toString))
    findStable(testDisplay1, Day2011.step).values.count(_ == '#') shouldBe 37

    Day2011.answer1 shouldBe 2324
  }

  it should "step2" in {
//    val d1 = Day2011.step2(testDisplay1)
////    println(d1.showDisplay()(_.toString))
//    val d2 = Day2011.step2(d1)
////    println(d2.showDisplay()(_.toString))
//    val d3 = Day2011.step2(d2)
////    println(d3.showDisplay()(_.toString))
    findStable(testDisplay1, step2).values.count(_ == '#') shouldBe 26

    Day2011.answer2 shouldBe 2068
  }

}
