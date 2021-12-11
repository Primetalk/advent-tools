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


  "Day2110alt" should "work on test input" in {
    input.map(Day2110alt.evalLine).sum should equal (26397)
  }
  "Day2110alt 2" should "work on test input" in {
    Day2110alt.answer2OfInput(input) should equal (288957)
  }
