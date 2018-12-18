package org.primetalk.advent

import org.primetalk.advent.Geom2dUtils.{PosOps, Position, Vector2d, VecOps}

import scala.reflect.ClassTag
import Geom2dUtils.directions8

// TODO: DisplayView - rotation on n*90; shift; constrain size; flip up/down
// TODO: PrintDisplay
// TODO: DrawDisplay on canvas (Scala.js)
// TODO: Remove state. Mutable array could be provided from outside as an implicit context
// TODO: Use refined type for array size,vector size.
case class Display[T: ClassTag](offset: Vector2d, size: Vector2d)(init: Option[() => Array[Array[T]]] = None) {


  // initial: () => T = () => {implicitly[Numeric[T]].zero}
  /** We only allocate array when it's needed*/
  lazy val array: Array[Array[T]] =
    init.getOrElse(() => Array.ofDim[T](size._2, size._1)).apply()

  def lineY(y: Int): Array[T] = array(y - minY)

  val minX: Int = offset._1
  val maxXplusExtra1: Int = minX + size._1
  val maxX: Int = maxXplusExtra1 - 1

  val minY: Int = offset._2
  val maxYplusExtra1: Int = minY + size._2
  val maxY: Int = maxYplusExtra1 - 1

  def xs = Range(minX, maxXplusExtra1)
  def ys = Range(minY, maxYplusExtra1)

  def isWithinRange(p: (Int, Int)): Boolean =
    p._1 >= minX && p._1 <= maxX &&
      p._2 >= minY && p._2 <= maxY

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

  def valuesAround(p: Position): Seq[T] = {
    directions8
      .map(_ + p)
      .filter(isWithinRange)
      .map(apply)
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
    array(p._2)(p._1)
  }

  def update(position: Position, v: T): Unit = {
    val p = position - offset
    array(p._2)(p._1) = v
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
    val d = Display[T](offset.transpose, size.transpose)()
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
    def arr = Array.fill(size._1)(value)
    for{
      j <- 0 until size._2
    } {
      array(j) = arr
    }
  }

  def showDisplay(colWidth: Int = 1)(show: T => String = _.toString): String = {
    (for{
      y <- ys
    } yield {
      lineY(y)
        .map(show)
        .map(_.padTo(colWidth, ' ')).mkString
    }).mkString("\n")
  }

  /** Transform this display according to cellular automaton rules. */
  def produceByLocalRules(rules: (T, Seq[T]) => T): Display[T] = {
    val d = new Display[T](offset, size)()
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

object Display {
  def readCharDisplay(lines: Seq[String]): Display[Char] = {
    val size = (lines.head.length, lines.length)
    val a = lines.map(_.toCharArray).toArray
    val d = Display[Char]((0,0), size)(Some(() => a))
    d
  }

  def eq[T](d1: Display[T], d2: Display[T]): Boolean = {
    d1.offset == d2.offset &&
    d1.size == d2.size &&
    d1.array.zip(d2.array)
      .forall{ case (a,b) => a.sameElements(b) }
  }
}
