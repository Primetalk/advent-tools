package org.primetalk.advent

import scala.util.Random

object Geom3dUtils {
  type Vector3d = (Int, Int, Int)
  type Position = Vector3d
  type Direction = Vector3d

  def parallelepipedByDiagonal(topLeft: Position, bottomRight: Position): Parallelepiped =
    Parallelepiped(topLeft, bottomRight - topLeft + (1, 1, 1))

  /** Origin is in top left corner. */
  case class Parallelepiped(topLeft: Position, size: Vector3d) {

    def bottomRight: Position = topLeft + size - (1, 1, 1)

    def isWithin(p: Position): Boolean = p match {
      case (x,y,z) =>
        x0 <= x && (x - x0) < size._1 &&
        y0 <= y && (y - y0) < size._2 &&
        z0 <= z && (z - z0) < size._3
    }

    def x0: Int = topLeft._1
    def y0: Int = topLeft._2
    def z0: Int = topLeft._3

    def x1: Int = x0 + size._1 - 1
    def y1: Int = y0 + size._2 - 1
    def z1: Int = z0 + size._3 - 1

    def isValid: Boolean =
      size.isAllPositive

    def randomPoint(rnd: Random): Position = {
      val s = size
      val i = rnd.nextInt(s._1)
      val j = rnd.nextInt(s._2)
      val k = rnd.nextInt(s._3)
      (i, j, k) + topLeft
    }

    def vertices: Seq[Position] =
      Seq(
        (x0,y0,z0),
        (x0,y0,z1),
        (x0,y1,z0),
        (x0,y1,z1),
        (x1,y0,z0),
        (x1,y0,z1),
        (x1,y1,z0),
        (x1,y1,z1)
      )

    def manhattanSize: Long = size.manhattanSize

    def coordinatePoints: Seq[Position] = Seq(topLeft, bottomRight)

    def intersect(other: Parallelepiped): Option[Parallelepiped] = {
      val othersVerticesInside = other.vertices.filter(isWithin)
      val verticesInsideOther = vertices.filter(other.isWithin)
      val vs = othersVerticesInside ++ verticesInsideOther
      vs.headOption.map(_ =>
        parallelepipedByDiagonal(
          (vs.map(_._1).min, vs.map(_._2).min, vs.map(_._3).min),
          (vs.map(_._1).max, vs.map(_._2).max, vs.map(_._3).max)
          )
      )
    }

    def divideIntoSmallerPieces(n: Int): Seq[Parallelepiped] = {
      if(size == (1,1,1))
        Seq(this)
      else
        (for {
          i <- 0 until n
          j <- 0 until n
          k <- 0 until n
          tl = (x0 + size._1 * i    / n, y0 + size._2 * j      / n, z0 + size._3 * k     / n)
          br = (x0 + size._1 * (i+1)/ n, y0 + size._2 * (j + 1)/ n, z0 + size._3 *(k + 1)/ n) - (1, 1, 1)
        } yield
          parallelepipedByDiagonal(tl, br))
            .filter(_.isValid)
            .filterNot(_ == this)
            .distinct
    }
  }

  type ManhattanPosition = Position
  type ManhattanParallelepiped = Parallelepiped
  /**
    * Manhattan affine transform is applied - rotate +45°, +45° and scale by sqrt(3)/3.
    * unit coordinate vectors are:
    *
    * i = ( 1, 1,-1)
    * j = ( 1,-1,-1)
    * k = ( 1,-1, 1)
    * (all have length sqrt(3) and are linearly independent)
    * NB! Basis vectors are not orthogonal!
    * These vectors are normal to all manhattan equidistant planes.
    */
  def  manhattanTransform(p: Position): ManhattanPosition = {
    (p._1 + p._2 - p._3, p._1 - p._2 - p._3, p._1 - p._2 + p._3)
  }

  def fromManhattan(p: ManhattanPosition): Position = {
    ((p._1 + p._3)/2 , (p._1 - p._2)/2, (p._3 - p._2)/2)
  }

  def manhattanSphere(p: Position, r: Int): ManhattanParallelepiped = {
    parallelepipedByDiagonal(manhattanTransform(p - (r,0,0)), manhattanTransform(p + (r,0,0)))
  }

  /** Finds bounding rectangle for a collection of points. */
  def boundingParallelepiped(positions: Seq[Position]): Parallelepiped = {
    val xs = positions.map(_._1)
    val ys = positions.map(_._2)
    val zs = positions.map(_._2)
    parallelepipedByDiagonal(
      topLeft = (xs.min, ys.min, zs.min),
      bottomRight = (xs.max, ys.max, zs.max)
    )
  }

  def meanAndStdDist(positions: Seq[Position]): (Position, Vector3d) = {
    val count = positions.size
    val xs = positions.map(_._1)
    val ys = positions.map(_._2)
    val zs = positions.map(_._2)
    val mean = (xs.sum/count, ys.sum/count, zs.sum/count)
    def stdDevCalc(els: Seq[Int], mean: Int): Int =
      math.sqrt(els.map(x => (x - mean) * (x - mean) ).sum/count).toInt

    val stdDev = (stdDevCalc(xs, mean._1), stdDevCalc(ys, mean._2), stdDevCalc(zs, mean._3))
    (mean, stdDev)
  }

  val origin: Position = (0, 0, 0)

  implicit class PosOps(p: Position) {
    def +(vector: Vector3d): Position =
      (p._1 + vector._1, p._2 + vector._2, p._3 + vector._3)
    def -(vector: Vector3d): Position =
      (p._1 - vector._1, p._2 - vector._2, p._3 - vector._3)

    def isAllNonNegative: Boolean =
      p._1 >= 0 && p._2 >= 0 && p._3 >= 0

    def isAllPositive: Boolean =
      p._1 >0 && p._2 >0 && p._3 >0

    def size: Long = (p._1 * p._1) + (p._2 * p._2) + (p._3 * p._3)
    def manhattanSize: Long = p._1.abs + p._2.abs + p._3.abs
  }

  implicit class VecOps(v: Vector3d) {

    def *(k: Int): Vector3d = v match {
      case (x, y, z) => (x * k, y * k, z * k)
    }

    def /(k: Int): Vector3d = v match {
      case (x, y, z) => (x / k, y / k, z / k)
    }
  }

  def manhattanDistance(v1: Vector3d, v2: Vector3d): Int = {
    (v1._1 - v2._1).abs +
      (v1._2 - v2._2).abs +
      (v1._3 - v2._3).abs
  }

}
