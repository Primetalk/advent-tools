package org.primetalk.advent

import Geom3dUtils._

class Geom3dUtilsTest extends BaseTest {

  behavior of "Geom3dUtilsTest"

  it should "manhattanSphere" in {
    println(manhattanSphere((0,0,0), 10).size)
  }

  it should "intersect" in {
    Parallelepiped(origin, (10,10,10)).intersect(
      Parallelepiped((5,5,5), (15,15,15))
    ) shouldBe Some(Parallelepiped((5,5,5), (10,10,10)))
    Parallelepiped(origin, (10,10,10)).intersect(
      Parallelepiped((-5,-5,-5), (-15,-15,-15))
    ) shouldBe None
    Parallelepiped(origin, (10,10,10)).intersect(
      Parallelepiped(origin, origin)
    ) shouldBe Some(Parallelepiped(origin, origin))
  }

  it should "divideIntoSmallerPieces" in {
    Parallelepiped(origin, (10,10,10)).divideIntoSmallerPieces(2) shouldBe
      Vector(Parallelepiped((0,0,0),(4,4,4)), Parallelepiped((0,0,5),(4,4,10)), Parallelepiped((0,5,0),(4,10,4)), Parallelepiped((0,5,5),(4,10,10)), Parallelepiped((5,0,0),(10,4,4)), Parallelepiped((5,0,5),(10,4,10)), Parallelepiped((5,5,0),(10,10,4)), Parallelepiped((5,5,5),(10,10,10)))
  }
}
