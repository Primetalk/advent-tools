package org.primetalk.advent.tools

import org.primetalk.advent.tools.Geom2dUtils.{Direction, PosOps, Position, Rectangle, VecOps, Vector2d, directions8, mainDirections}

import scala.annotation.tailrec
import scala.reflect.ClassTag

// TODO: DisplayView - rotation on n*90; shift; constrain size; flip up/down
// DONE showDisplay
// TODO: DrawDisplay on canvas (Scala.js)
// TODO: Remove state. Mutable array could be provided from outside as an implicit context
// TODO: Use refined type for array size,vector size.
case class Display[T: ClassTag](offset: Vector2d, size: Vector2d)(init: Option[() => Array[Array[T]]] = None) {
  lazy val rect: Rectangle = Rectangle(offset, size)

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

  def isWithinRange(p: Position): Boolean =
    p._1 >= minX && p._1 <= maxX &&
      p._2 >= minY && p._2 <= maxY

  def toPositionPredicate(predicate: T => Boolean): Position => Boolean =
    p => isWithinRange(p) && predicate(apply(p))

  def isElementEqualTo(p: Position, el: T): Boolean =
    isWithinRange(p) && apply(p) == el

  def adjacentPositions(p: Position): List[Position] =
    mainDirections.map(_ + p).filter(isWithinRange)

  def mainPositionsAround(p: Position): List[Position] =
    adjacentPositions(p)

  def positionsAround(p: Position): List[Position] =
    directions8.map(_ + p).filter(isWithinRange)

  def points: Seq[Position] = pointsLeftToRightTopToBottomYGrowsDown

  def pointsLeftToRightTopToBottomYGrowsDown: Seq[(Int, Int)] =
    for{
      j <- ys
      i <- xs
    } yield (i, j)

  def innerPoints: Seq[Position] =
    for{
      j <- Range(minY + 1, maxY - 1)
      i <- Range(minX + 1, maxX - 1)
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
    positionsAround(p)
      .map(apply)
  }

  /** Performs search in the given direction.
    * If found, returns the position.
    */
  @tailrec
  final def searchInDirection(pos: Position, dir: Direction, pred: T => Boolean): Option[Position] = {
    val nextPos = pos + dir
    if(isWithinRange(nextPos)){
      val c = apply(nextPos)
      if(pred(c))
        Some(nextPos)
      else
        searchInDirection(nextPos, dir, pred)
    } else
      None
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
    try {
      array(p._2)(p._1)
    } catch {
      case e: IndexOutOfBoundsException =>
        throw new IndexOutOfBoundsException(s"$position does not belong to a rectangle at $offset with size $size")
    }
  }

  def get(position: Position): Option[T] =
    if(isWithinRange(position))
      Some(apply(position))
    else
      None
  /**
    * Updates the position if it is present. Ignores otherwise.
    * This might be helpful if we search for some condition in a given area and don't
    * want to store large array of data.
    */
  def safeUpdate(position: Position, v: T): Boolean = {
    val wasUpdated = isWithinRange(position)
    val p = position - offset
    if(wasUpdated) {
      array(p._2)(p._1) = v
    }
    wasUpdated
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

    @tailrec
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

  def fill(f: Position => T): Unit = renderFunction(f)

  /** Fill display with the given value.
    * Faster than renderFunction(_ => value)
    * */
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

  /** Transform this display according to cellular automaton rules. */
  def produceByLocalRulesFromMainDirections(rules: (T, Seq[T]) => T): Display[T] = {
    val d = new Display[T](offset, size)()
    for{
      p <- points
      v = mainPositionsAround(p).map(apply)
    } {
      d(p) = rules(apply(p), v)
    }
    d
  }

  def produceByGlobalRules(rules: ((Int, Int), T) => T): Display[T] = {
    val d = new Display[T](offset, size)()
    for{
      p <- points
      c = apply(p)
      next = rules(p,c)
    } {
      d(p) = next
    }
    d
  }

  def map[B: ClassTag](f: T => B): Display[B] = {
    val a: Array[Array[B]] = array.map(_.map(f).toArray)
    new Display[B](offset, size)(Some(() => a))
  }

  def flipY: Display[T] = {
    val a: Array[Array[T]] = array.reverse
    new Display[T]((offset._1, -offset._2), size)(Some(() => a))
  }
}

object Display {
  def apply[T: ClassTag](rect: Rectangle): Display[T] = {
    new Display[T](rect.topLeft, rect.size)()
  }

  def readCharDisplay(lines: Seq[String], emptyChar: Char = 0): Display[Char] = {
    val width = lines.map(_.length).max
    val size = (width, lines.length)
    val a = lines.map{l =>
      val arr = l.toCharArray
      if(arr.length == width)
        arr
      else {
        val arr2 = Array.fill[Char](width)(emptyChar)
        arr.copyToArray(arr2)
        arr2
      }
    }.toArray
    val d = Display[Char]((0,0), size)(Some(() => a))
    d
  }

  def eq[T](d1: Display[T], d2: Display[T]): Boolean = {
    d1.offset == d2.offset &&
    d1.size == d2.size &&
    d1.array.zip(d2.array)
      .forall{ case (a,b) => a.sameElements(b) }
  }

  def showPoints(points: Seq[Position], pointChar: Char = '.'): Display[Char] = {
    val rect = Geom2dUtils.boundingRect(points)
    val d = Display[Char](rect)
    d.fillAll(' ')
    points.foreach(p => d(p) = pointChar)
    d
  }

  def of[T: ClassTag](offset: Vector2d, size: Vector2d)(f: Position => T): Display[T] = {
    val d = Display[T](offset, size)()
    d.fill(f)
    d
  }
}
