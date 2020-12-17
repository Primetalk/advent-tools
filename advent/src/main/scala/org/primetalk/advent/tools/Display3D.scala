package org.primetalk.advent.tools

import org.primetalk.advent.tools.Geom2dUtils.Position
import org.primetalk.advent.tools.Geom3dUtils.{Vector3d, directions26, Position => Position3d, PosOps}

import scala.reflect.ClassTag

class Display3D[T:ClassTag](val offset3d: Vector3d, val size3d: Vector3d) {
  val inner: Display[T] = Display[T](convertPositionToInner(offset3d), (size3d._1 * size3d._3, size3d._2))()

  def writeLayer(z: Int, d: Display[T]): Unit = {
    val points2d = d.points
    points2d.foreach{ p2d =>
      update((p2d._1, p2d._2, z), d(p2d))
    }
  }

  def convertPositionToInner(p: Vector3d): Position = ((p._1-offset3d._1) + (p._3-offset3d._3) * size3d._1, p._2-offset3d._2)

  def apply(p: Vector3d): T = inner.apply(convertPositionToInner(p))
  def update(p: Vector3d, v: T): Unit = inner.update(convertPositionToInner(p), v)
  def deepCopy: Display3D[T] = {
    val res = new Display3D[T](offset3d, size3d)
    res.inner.renderFunction(p => inner(p))
    res
  }
  val minX: Int = offset3d._1
  val maxXplusExtra1: Int = minX + size3d._1
  val maxX: Int = maxXplusExtra1 - 1

  val minY: Int = offset3d._2
  val maxYplusExtra1: Int = minY + size3d._2
  val maxY: Int = maxYplusExtra1 - 1

  val minZ: Int = offset3d._3
  val maxZplusExtra1: Int = minZ + size3d._3
  val maxZ: Int = maxZplusExtra1 - 1

  def xs: List[Int] = (minX until maxXplusExtra1).toList
  def ys: List[Int] = (minY until maxYplusExtra1).toList
  def zs: List[Int] = (minZ until maxZplusExtra1).toList

  def points: List[Vector3d] =
    for{
      k <- zs
      j <- ys
      i <- xs
    } yield (i,j,k)

  def isWithinRange(p: Vector3d): Boolean =
    p._1 >= minX && p._1 <= maxX &&
      p._2 >= minY && p._2 <= maxY &&
      p._3 >= minZ && p._3 <= maxZ

  /** Draws the function on this display. */
  def renderFunction(f: Vector3d => T): Unit = {
    for{
      p <- points
    } {
      this(p) = f(p)
    }
  }

  def values: List[T] = points.map(apply)

  def positionsAround(p: Position3d): List[Position3d] =
    directions26.map(_ + p).filter(isWithinRange)

  def valuesAround(p: Position3d): Seq[T] = {
    positionsAround(p)
      .map(apply)
  }

  /** Transform this display according to cellular automaton rules. */
  def produceByLocalRules(rules: (T, Seq[T]) => T): Display3D[T] = {
    val d = new Display3D[T](offset3d, size3d)
    for{
      p <- points
      v = valuesAround(p)
      next = rules(apply(p), v)
    } {
      d(p) = next
    }
    d
  }

}
