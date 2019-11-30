package org.primetalk.advent2018

import org.primetalk.advent.tools.Geom3dUtils._

class Geom3dUtilsTest extends BaseTest {

  behavior of "Geom3dUtilsTest"

  it should "manhattanSphere" in {
    println(manhattanSphere(origin, 10).size)
  }

  val size5: Position = (5, 5, 5)

  val size10: Position = (10, 10, 10)

  it should "intersect" in {
    Parallelepiped(origin, size10).intersect(
      Parallelepiped(size5, size10)
    ) shouldBe Some(Parallelepiped(size5, size5))
    Parallelepiped(origin, size10).intersect(
      Parallelepiped((-15,-15,-15), size10)
    ) shouldBe None
    val size1 = (1, 1, 1)
    Parallelepiped(origin, size10).intersect(
      Parallelepiped(origin, size1)
    ) shouldBe Some(Parallelepiped(origin, size1))
  }

  it should "divideIntoSmallerPieces" in {
    Parallelepiped(origin, size10).divideIntoSmallerPieces(2) shouldBe
      Vector(Parallelepiped((0,0,0),size5), Parallelepiped((0,0,5),size5), Parallelepiped((0,5,0),size5), Parallelepiped((0,5,5),size5), Parallelepiped((5,0,0),size5), Parallelepiped((5,0,5),size5), Parallelepiped((5,5,0),size5), Parallelepiped((5,5,5),size5))
  }
}
