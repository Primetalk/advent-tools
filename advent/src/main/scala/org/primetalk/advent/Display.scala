package org.primetalk.advent

import org.primetalk.advent.Geom2dUtils.{PosOps, Position, Vector, VecOps}

import scala.reflect.ClassTag

case class Display[T: ClassTag](offset: Vector, size: Vector) {
  // initial: () => T = () => {implicitly[Numeric[T]].zero}
  /** We only allocate array when it's needed*/
  lazy val array: Array[Array[T]] = Array.ofDim[T](size._1, size._2)

  val minX: Int = offset._1
  val maxXplusExtra1: Int = minX + size._1
  val maxX: Int = maxXplusExtra1 - 1

  val minY: Int = offset._2
  val maxYplusExtra1: Int = minY + size._2
  val maxY: Int = maxYplusExtra1 - 1

  def xs = Range(minX, maxXplusExtra1)
  def ys = Range(minY, maxYplusExtra1)

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

  /** Enumerates all positions on edges.
    * O(N+M)
    * The order is not guaranteed.
    * It might be considered as Set.
    */
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
  def inclusiveRectSum(topLeft: Position, bottomRight: Position)(implicit num: Numeric[T]): T = {
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

  //    d.array = array.transpose
  def transpose: Display[T] = {
    val d = Display[T](offset.transpose, size.transpose)
    for{
      p <- points
      pp = p.transpose
    } {
      d(pp) = apply(p)
    }
    d
  }

  /** Draws the function on this display. */
  def renderFunction(f: Position => T): Unit = {
    for{
      p <- points
    } {
      this(p) = f(p)
    }
  }

  /** Fill display with the given value.*/
  def fillAll(value: => T): Unit = {
    val arr = Array.fill(size._2)(value)
    for{
      y <- ys
    } {
      array(y) = arr
    }
  }
}
