package org.primetalk.advent3.tools

import org.primetalk.advent3.tools.Geom2dUtils._

class Geom2dSpec extends UnitSpec:
  "LineSegment" should "calculate relative 1" in {
    val noTransform = LineSegment((0,0), DirVector(Right, 1))
    val p = (10, 20)
    noTransform.relative(p) should equal (p)
    noTransform.fromRelative(p) should equal (p)
  }
  "LineSegment" should "calculate relative 2" in {
    val ls = LineSegment((5,5), DirVector(Up, 10))
    val p = (10, 20)
    ls.relative(p) should equal ((15, -5))
    ls.fromRelative(ls.relative(p)) should equal (p)
  }
  "LineSegment" should "calculate relative 3" in {
    val ls = LineSegment((5,5), DirVector(Left, 10))
    val p = (10, 20)
    ls.relative(p) should equal ((-5, -15))
    ls.fromRelative(ls.relative(p)) should equal (p)
  }
  "LineSegment" should "calculate relative 4" in {
    val ls = LineSegment((5,5), DirVector(Down, 10))
    val p = (10, 20)
    ls.relative(p) should equal ((-15, 5))
    ls.fromRelative(ls.relative(p)) should equal (p)
  }
