package org.primetalk.advent2018


class Day13Test extends BaseTest {

  behavior of "Day13Test"

  it should "nextDirectionOnJunction" in {
    import org.primetalk.advent.tools.Geom2dUtils.{ Down => DisplayDown, Up => DisplayUp, _ }
    import Day13._
    Day13.nextDirectionOnJunction(MapUp, 0) shouldBe Left
    Day13.nextDirectionOnJunction(Left, 0) shouldBe MapDown
    Day13.nextDirectionOnJunction(MapDown, 0) shouldBe Right
    Day13.nextDirectionOnJunction(Right, 0) shouldBe MapUp
    Day13.nextDirectionOnJunction(Right, 1) shouldBe Right
    Day13.nextDirectionOnJunction(Right, 2) shouldBe MapDown
  }

}
