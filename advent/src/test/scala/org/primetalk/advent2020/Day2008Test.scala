package org.primetalk.advent2020

import org.primetalk.advent2020.Day2008._

class Day2008Test extends BaseTest {

  behavior of "Day2008Test"

  it should "answers" in {
    answer1 shouldBe 1949
    answer2 shouldBe 2092
  }

  val programExample: IndexedSeq[Op] = IndexedSeq(
    nop(+0),
    acc(+1),
    jmp(+4),
    acc(+3),
    jmp(-3),
    acc(-99),
    acc(+1),
    jmp(-4),
    acc(+6),
  )
  it should "parseProgram" in {
    parseProgram(
      """nop +0
        |acc +1
        |jmp +4
        |acc +3
        |jmp -3
        |acc -99
        |acc +1
        |jmp -4
        |acc +6
        |""".stripMargin) shouldBe(programExample)

  }

}
