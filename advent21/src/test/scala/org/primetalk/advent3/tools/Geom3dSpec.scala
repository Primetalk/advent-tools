package org.primetalk.advent3.tools

import org.primetalk.advent3.tools.Geom3dUtils._

class Geom3dSpec extends UnitSpec:
  "InclusiveRange" should "exclude" in {
    (0, 2).exclude((0, 1)) should equal (List((2,2)))
  }
  val p1 = parallelepipedFromRanges((0,1), (0, 1), (0,1))
  val p2 = parallelepipedFromRanges((0,2), (0, 2), (0,2))
  "Parallelepiped" should "subtract" in {
    p2.intersect(p1) should equal(Some(p1))
    val lst = p2.subtract(p1) 
    lst should have size(7)//(Nil)
    lst.map(_.volume).sum should equal (p2.volume - p1.volume)
    val in = lst.flatMap(sp =>
      sp.intersect(p1).toList) 
    in shouldBe empty
  }

  val p3 = parallelepipedFromRanges((-33,18), (-35, 11), (-49,2))
  val p4 = parallelepipedFromRanges((-14, 32), (5, 49), (-42,5))
  val p5 = parallelepipedFromRanges((-28, 18), (-38, 10), (-14,33))

  "Parallelepiped 3,4" should "subtract" in {
    val Some(i) = p3.intersect(p4)
    p3.intersect(i) should equal(Some(i))
    p4.intersect(i) should equal(Some(i))

    val lst34 = p3.subtract(p4)
    lst34 should have size(7)//(Nil)
    val in = lst34.flatMap(_.intersect(p4).toList) 
    in shouldBe empty
    
    lst34.map(_.volume).sum should equal (p3.volume - i.volume)

    p4.subtract(p3) should have size(7)

    val lst35 = lst34.flatMap(_.subtract(p5))
    lst35.foreach{p => 
      assert(p3.containsCompletely(p))
    }
    val lst35invalid = lst35.filter(_.intersect(p5).isDefined)

    lst35invalid shouldBe empty
    
  }
