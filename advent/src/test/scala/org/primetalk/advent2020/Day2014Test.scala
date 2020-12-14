package org.primetalk.advent2020

import org.primetalk.advent.tools.BaseTest
import org.primetalk.advent2020.Day2014.{State, applyMask, applyMask2, evalAll, mask, parseBitmask, parseProgram}

class Day2014Test extends BaseTest {

  behavior of "Day2014Test"

  it should "task1" in {

    val input =
      """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
        |mem[8] = 11
        |mem[7] = 101
        |mem[8] = 0
        |""".stripMargin
    val program = parseProgram(input)
    val last = evalAll(program, State())
    last.memory.values.sum shouldBe 165L
  }

  it should "bitmask" in {
    val m1 = parseBitmask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X".toList)
    println(s"set1=${m1.set1}, set0 = ${m1.set0}")
    applyMask(m1)(11) shouldBe 73
  }
  it should "task1 example" in {
    val input =
      """mask = 1X011000110010X0X100101XX111X1X100X1
        |mem[15042] = 1048663
        |mem[46450] = 906445926
        |mem[65481] = 325
        |mem[8090] = 145570
        |mem[46447] = 2513""".stripMargin
    val program = parseProgram(input)
    val last = evalAll(program, State())
    last.memory(65481) shouldBe 41012209489L
  }

  it should "bitmask2" in {
    applyMask2(mask(0,0, IndexedSeq(1L, 2L)))(0L) shouldBe List(0L, 1L, 2L, 3L)
  }
}
