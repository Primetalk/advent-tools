package org.primetalk.advent2018

import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

class DisplayTest extends FlatSpec with PropertyChecks with Generators {

  behavior of "DisplayTest"

  it should "edges" in
    forAll(intDisplayGen){ display: Display[Int] =>
      val edgePointValues = display.edges.map(display.apply).toSet
      val edgePointValues2= display.valuesOnEdges
      edgePointValues == edgePointValues2
    }

}
