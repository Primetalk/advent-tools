package org.primetalk.advent2018

class Day19Test extends BaseTest {

  behavior of "Day19Test"

  it should "eval" in {
    new Day19 {
      val program: String =
        """#ip 0
          |seti 5 0 1
          |seti 6 0 2
          |addi 0 1 0
          |addr 1 2 3
          |setr 1 0 0
          |seti 8 0 4
          |seti 9 0 5
          |""".stripMargin
      val lines: Seq[String] = program.split("\n")

      val finalRegisters: Registers = eval(inputProgram.toVector)(Registers(Seq(0,0,0,0,0,0), initialIpSelector, 0))
      finalRegisters.values shouldBe Seq(6, 5, 6, 0, 0, 9)

    }
  }

}
