package org.primetalk.advent2019

import org.scalatest.{FlatSpec, Matchers}
import Day22._

class Day22Test extends FlatSpec with Matchers {

  behavior of "Day22Test"

  it should "power" in {
    val residualFormula = ResidualFormula(0, 4, 497)
    val res = residualFormula.power(13)
    res.m shouldBe 0L
    res.offset shouldBe 445L
  }

  it should "inverse" in {
    inverse(2, 5) shouldBe 3
    inverse(3, 7) shouldBe 5
    inverse(3, 13) shouldBe 9
    inverse(40556623190454L, 119315717514047L) shouldBe 110934924590471L
    BigInt(40556623190454L)*110934924590471L %119315717514047L shouldBe BigInt(1)
  }

  "modPower" should "work" in {
    modPower(4,13,497) shouldBe BigInt(445)
  }


}
