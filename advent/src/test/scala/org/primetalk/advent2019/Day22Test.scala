package org.primetalk.advent2019

import Day22._
import org.primetalk.advent.tools.ModuloArithmetics.ModuloField

class Day22Test extends BaseTest {

  behavior of "Day22Test"

  it should "power" in {
    val residualFormula = ResidualFormula(BigInt(0), BigInt(4), ModuloField(BigInt(497)))
    val res = residualFormula.power(13)
    res.m shouldBe 0L
    res.offset shouldBe 445L
  }

  it should "inverse" in {
    ModuloField(5).inverse(2) shouldBe 3
    ModuloField(7).inverse(3) shouldBe 5
    ModuloField(13).inverse(3) shouldBe 9
    ModuloField(119315717514047L).inverse(40556623190454L) shouldBe 110934924590471L
    BigInt(40556623190454L)*110934924590471L %119315717514047L shouldBe BigInt(1)
  }

  "modPower" should "work" in {
    ModuloField(497).power(4,13) shouldBe BigInt(445)
  }


}
