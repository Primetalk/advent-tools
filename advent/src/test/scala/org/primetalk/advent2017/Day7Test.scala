package org.primetalk.advent2017

import org.scalatest.FlatSpec

class Day7Test extends FlatSpec {


  import Day7._
  "testAnswer2" should "run" in {
    val in = """pbga (66)
      |xhth (57)
      |ebii (61)
      |havc (66)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq
      |tknk (41) -> ugml, padx, fwft
      |jptl (61)
      |ugml (68) -> gyxo, ebii, jptl
      |gyxo (61)
      |cntj (57)
      |""".stripMargin.split('\n').toSeq

    val inLines = in.map(parse)
    val root = buildTree(inLines)
    val Seq(60) = searchUnbalancedProgram(root, 0)
  }

}
