package org.primetalk.advent.tools

import org.primetalk.advent.tools.ModuloArithmetics.modPower

class ModuloArithmeticsTest extends BaseTest {

  behavior of "ModuloArithmetics"

  it should "modPower" in {
    modPower(4,13,497) shouldBe BigInt(445)
  }

}
