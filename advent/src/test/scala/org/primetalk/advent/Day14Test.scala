package org.primetalk.advent

import Day14._

class Day14Test extends BaseTest {

  behavior of "Day14Test"

  /*
    If the Elves think their skill will improve after making 9 recipes,
     the scores of the ten recipes after the first nine on the scoreboard would be
     5158916779 (highlighted in the last line of the diagram).
    After 5 recipes, the scores of the next ten would be 0124515891.
    After 18 recipes, the scores of the next ten would be 9251071085.
    After 2018 recipes, the scores of the next ten would be 5941429882.
  */
  it should "answer1" in {
    produceAfterN(9) shouldBe "5158916779"
    produceAfterN(5) shouldBe "0124515891"
    produceAfterN(18) shouldBe "9251071085"
    produceAfterN(2018) shouldBe "5941429882"
  }

   /*
    51589 first appears after 9 recipes.
    01245 first appears after 5 recipes.
    92510 first appears after 18 recipes.
    59414 first appears after 2018 recipes.
   */
  it should "answer2" in {
    produceVector("51589".toCharArray.toVector, 0, 1, Vector('3','7')) shouldBe 9
    produceVector("01245".toCharArray.toVector, 0, 1, Vector('3','7')) shouldBe 5
    produceVector("92510".toCharArray.toVector, 0, 1, Vector('3','7')) shouldBe 18
    produceVector("59414".toCharArray.toVector, 0, 1, Vector('3','7')) shouldBe 2018
  }
}
