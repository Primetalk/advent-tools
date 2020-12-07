package org.primetalk.advent2020

import org.primetalk.advent2020.Day2007._

class Day2007Test extends BaseTest {

  behavior of "Day2007Test"

  import fastparse._
  import SingleLineWhitespace._
  it should "answer1" in {
    answer1 shouldBe 289
  }
  it should "answer2" in {
    answer2 shouldBe 30055
  }
  it should "parse bag name" in {
    parse("a b bags", Day2007.bag(_)).get.value shouldBe Bag("a b")
  }
  it should "pass test example" in {
    val rules = """faded blue bags contain no other bags.
      |dotted black bags contain no other bags.
      |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
      |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
      |shiny gold bag contain 1 dark olive bag, 2 vibrant plum bags.
      |""".stripMargin
      .split('\n')
      .toList
      .map(parseRule)
  }
}
