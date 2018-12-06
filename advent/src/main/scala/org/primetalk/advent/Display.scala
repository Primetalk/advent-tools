package org.primetalk.advent

import org.primetalk.advent.Geom2dUtils.{PosOps, Position, Vector}

import scala.reflect.ClassTag

case class Display[T: Numeric: ClassTag](offset: Vector, size: Vector) {

  val minX: Int = offset._1

  val maxX1: Int = minX + size._1
  val maxX: Int = maxX1 - 1

  val minY: Int = offset._2
  val maxY1: Int = minY + size._2
  val maxY: Int = maxY1 - 1

  def points: Seq[Position] =
    for{
      j <- minY until maxY1
      i <- minX until maxX1
    } yield (i, j)

  def values: Seq[T] =
    for{
      j <- minY until maxY1
      i <- minX until maxX1
    } yield apply(i, j)

  def valuesOnEdges: Set[T] =
    {
      val jValues = for {
        j <- minY until maxY1
        v <- Seq(apply((minX, j)), apply((maxX, j)))
      } yield v

      val iValues = for {
        i <- minX until maxX1
        v <- Seq(apply((i, minY)), apply((i, maxY)))
      } yield v

      iValues.toSet ++ jValues.toSet
    }


  lazy val array: Array[Array[T]] = Array.ofDim[T](size._1, size._2)

  def apply(position: Position): T = {
    val p = position - offset
    array(p._1)(p._2)
  }

  def update(position: Position, v: T): Unit = {
    val p = position - offset
    array(p._1)(p._2) = v
  }

  /** Sum of all elements in rect inclusive boundaries. */
  def inclusiveRectSum(topLeft: Position, bottomRight: Position): T = {
    val num = implicitly[Numeric[T]]
    val tl = topLeft - offset
    val br = bottomRight - offset

    def go(i: Int, j: Int, accum: T): T = {
      if (j > br._2)
        accum
      else {
        if (i > br._1)
          go(tl._1, j + 1, accum)
        else
          go(i + 1, j, num.plus(accum, array(i)(j)))
      }
    }

    go(tl._1, tl._2, num.zero)
  }

}
