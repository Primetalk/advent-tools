package org.primetalk.advent

import org.primetalk.advent.Geom2dUtils.{PosOps, Position, Vector}

import scala.reflect.ClassTag

case class Display[T: Numeric: ClassTag](offset: Vector, size: Vector) {

  /** We only allocate array when it's needed*/
  lazy val array: Array[Array[T]] = Array.ofDim[T](size._1, size._2)

  val minX: Int = offset._1
  val maxXplusExtra1: Int = minX + size._1
  val maxX: Int = maxXplusExtra1 - 1

  val minY: Int = offset._2
  val maxYplusExtra1: Int = minY + size._2
  val maxY: Int = maxYplusExtra1 - 1

  def xs = Range(minX, maxX)
  def ys = Range(minY, maxY)

  def points: Seq[Position] =
    for{
      j <- ys
      i <- xs
    } yield (i, j)

  def values: Seq[T] =
    for{
      j <- ys
      i <- xs
    } yield apply(i, j)

  def valuesOnEdges: Set[T] =
    {
      val jValues = for {
        j <- ys
        v <- Seq(apply((minX, j)), apply((maxX, j)))
      } yield v

      val iValues = for {
        i <- xs
        v <- Seq(apply((i, minY)), apply((i, maxY)))
      } yield v

      iValues.toSet ++ jValues.toSet
    }

  /** The order is not guaranteed. It might be considered as Set*/
  def edges: Seq[Position] = {
    if(maxX < minX || maxY < minY)
      Seq()
    else if(maxX == minX)
      ys.map((minX, _))
    else if(maxY == minY)
      xs.map((_, minY))
    else
      xs.map((_, minY)) ++
        xs.map((_, maxY)) ++
        (minY + 1).until(maxY).map((minX, _)) ++
        (minY + 1).until(maxY).map((maxX, _))
  }
  /** Enumerates all positions on edges.
    * O(N+M)*/
//  def edges: Set[Position] =
//    {
//      if(minX > maxX1 || minY > maxY1) Set()
//      val jValues = for {
//        j <- ys
//        v <- Seq(apply((minX, j)), apply((maxX1, j)))
//      } yield v
//
//      val iValues = for {
//        i <- xs
//        v <- Seq(apply((i, minY)), apply((i, maxY1)))
//      } yield v
//
//      iValues.toSet ++ jValues.toSet
//    }

  def apply(position: Position): T = {
    val p = position - offset
    array(p._1)(p._2)
  }

  def update(position: Position, v: T): Unit = {
    val p = position - offset
    array(p._1)(p._2) = v
  }

  /** Sum of all elements in rect inclusive boundaries.
    * Rectangle should be within display boundaries.
    */
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
