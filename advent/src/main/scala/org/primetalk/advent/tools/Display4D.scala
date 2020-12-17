package org.primetalk.advent.tools

import org.primetalk.advent.tools.Geom4dUtils.{VecOps, Vector4d, directions80}

import scala.reflect.ClassTag

class Display4D[T:ClassTag](val offset4d: Vector4d, val size4d: Vector4d) {
  val array: Array[Display3D[T]] =
    (for{
      w <- 0 until size4d._4
    } yield {
      new Display3D[T](offset4d.part3d, size4d.part3d)
    }).toArray

  def writeCubeInPlace(w: Int, d: Display3D[T]): Unit =
    array(w - offset4d._4) = d

  def getIndex4(p: Vector4d): Int = p._4 - offset4d._4

  def apply(p: Vector4d): T = array(getIndex4(p)).apply(p.part3d)
  def update(p: Vector4d, v: T): Unit = array(getIndex4(p)).update(p.part3d, v)

  val minX: Int = offset4d._1
  val maxXplusExtra1: Int = minX + size4d._1
  val maxX: Int = maxXplusExtra1 - 1

  val minY: Int = offset4d._2
  val maxYplusExtra1: Int = minY + size4d._2
  val maxY: Int = maxYplusExtra1 - 1

  val minZ: Int = offset4d._3
  val maxZplusExtra1: Int = minZ + size4d._3
  val maxZ: Int = maxZplusExtra1 - 1

  val minW: Int = offset4d._4
  val maxWplusExtra1: Int = minW + size4d._4
  val maxW: Int = maxWplusExtra1 - 1

  def xs: List[Int] = (minX until maxXplusExtra1).toList
  def ys: List[Int] = (minY until maxYplusExtra1).toList
  def zs: List[Int] = (minZ until maxZplusExtra1).toList
  def ws: List[Int] = (minW until maxWplusExtra1).toList

  def points: List[Vector4d] =
    for{
      l <- ws
      k <- zs
      j <- ys
      i <- xs
    } yield (i,j,k,l)

  def isWithinRange(p: Vector4d): Boolean =
    p._1 >= minX && p._1 <= maxX &&
      p._2 >= minY && p._2 <= maxY &&
      p._3 >= minZ && p._3 <= maxZ &&
      p._4 >= minW && p._4 <= maxW

  /** Draws the function on this display. */
  def renderFunction(f: Vector4d => T): Unit = {
    for{
      p <- points
    } {
      this(p) = f(p)
    }
  }

  def values: List[T] = points.map(apply)

  def positionsAround(p: Vector4d): List[Vector4d] =
    directions80.map(_ + p).filter(isWithinRange)

  def valuesAround(p: Vector4d): Seq[T] = {
    positionsAround(p)
      .map(apply)
  }

  /** Transform this display according to cellular automaton rules. */
  def produceByLocalRules(rules: (T, Seq[T]) => T): Display4D[T] = {
    val d = new Display4D[T](offset4d, size4d)
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
