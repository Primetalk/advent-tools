package org.primetalk.advent2018

import org.scalatest.{FlatSpec, Matchers}
import Day7._

class Day7Test extends FlatSpec with Matchers {

  behavior of "Day7Test"
  val exampleEdges: Array[(Char, Char)] =
    """Step C must be finished before step A can begin.
      |Step C must be finished before step F can begin.
      |Step A must be finished before step B can begin.
      |Step A must be finished before step D can begin.
      |Step B must be finished before step E can begin.
      |Step D must be finished before step E can begin.
      |Step F must be finished before step E can begin.
      |""".stripMargin.split('\n').map(parse)

  it should "perform example in 15 steps" in {
    runWork(exampleEdges, 2, _ - 'A' + 1) should be(15)
  }

  it should "one worker should yield the same string" in {
    runWork(exampleEdges, 1, _ => 1) should be(6)
  }

  // BGKDMJCNEQRSTUZWHYLPAFIVXO
  it should "one worker should yield the same string for prod" in {
    runWork(input, 1, _ => 1) should be(26)
  }

}
