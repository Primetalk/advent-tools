package org.primetalk.advent2019

import org.primetalk.advent.tools.SequenceUtils
import org.primetalk.advent2019.ImmutableMoon.MoonState

class Day12Test extends BaseTest {

  behavior of "Day12Test"

  val testMoons: IndexedSeq[MoonState] = IndexedSeq(
    MoonState((-1, 0, 2)),
    MoonState((2, -10, -7)),
    MoonState((4, -8, 8)),
    MoonState((3, 5, -1))
  )

  it should "find loop" in {
    val s0TortoiseHare = Array(
      new MutableMoon("s0", testMoons),
      new MutableMoon("tortoise", testMoons),
      new MutableMoon("hare", testMoons))
    val (start, loop) = SequenceUtils.floydMutable(s0TortoiseHare)(MutableMoon.eq, MutableMoon.copy)(_.timeStep())
    println(s"s:$start, l:$loop")
    start + loop shouldBe 2772
  }

}
