package org.primetalk.advent2018

import org.primetalk.advent.tools.Display

class DisplayTest extends BaseTest with Generators {

  behavior of "DisplayTest"

  it should "edges" in
    forAll(intDisplayGen){ display: Display[Int] =>
      val edgePointValues = display.edges.map(display.apply).toSet
      val edgePointValues2= display.valuesOnEdges
      edgePointValues == edgePointValues2
    }

}
