package org.primetalk.advent

import org.primetalk.advent.GraphUtils._
import org.primetalk.advent.Geom2dUtils._

class GraphUtilsTest extends BaseTest {

  behavior of "GraphUtilsTest"

  it should "findShortestPaths" in {
    distance(d2graph4dir)(origin, (5,5)) shouldBe 10
    distance(d2graph8dir)(origin, (5,5)) shouldBe 5
//    distance(d2graph4dir)(origin, (100,100)) shouldBe 200 // It takes some time
  }

}
