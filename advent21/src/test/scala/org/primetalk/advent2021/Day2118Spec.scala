package org.primetalk.advent2021

import org.primetalk.advent3.tools.UnitSpec
import Day2118._

class Day2118Spec extends UnitSpec:

  "Day2118" should "work on test input" in {
"""[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
""".split('\n').toIndexedSeq.map(Day2118.parseNumber).reduce(_ + _) should equal (
      parseNumber("[[[[3,0],[5,3]],[4,4]],[5,5]]"))
  }

    "Day2118" should "work on test input 6" in {
"""[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]
""".split('\n').toIndexedSeq.map(Day2118.parseNumber).reduce(_ + _) should equal (
      parseNumber("[[[[5,0],[7,4]],[5,5]],[6,6]]"))
  }

    "Day2118" should "work on the larger example" in {
"""[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]
""".split('\n').toIndexedSeq.map(Day2118.parseNumber).reduce(_ + _) should equal (
      parseNumber("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))
  }

  "magnitude" should "work on examples" in {
    parseNumber("[[1,2],[[3,4],5]]").magnitude should equal (BigInt(143))
  }
