package org.primetalk.advent2021

import org.primetalk.advent3.tools.UnitSpec

class Day2110Spec extends UnitSpec:

  val input = 
    """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
""".split('\n').toIndexedSeq // 
  "Day2110" should "work on test input" in {
    input.map(Day2110.evalLine).sum should equal (26397)
  }
