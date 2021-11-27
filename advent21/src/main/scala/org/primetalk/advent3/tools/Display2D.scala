package org.primetalk.advent3.tools

import org.primetalk.advent3.tools.Geom2dUtils.*

import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
  * Display is oriented (x: left->right, y: top->down.).
  *
  * @param offset - top-left corner. ys increase down, xs increase right
  */
// TODO: DisplayView - rotation on n*90; shift; constrain size; flip up/down
// DONE showDisplay
// TODO: DrawDisplay on canvas (Scala.js)
// TODO: Remove state. Mutable array could be provided from outside as an implicit context
// TODO: Use refined type for array size,vector size.
case class Display2D[T: ClassTag](offset: Vector2d, size: Vector2d)(init: Option[() => Array[Array[T]]] = None):
  /** We only allocate array when it's needed*/
  lazy val array: Array[Array[T]] =
    init.getOrElse(() => Array.ofDim[T](size._2, size._1)).apply()

  def apply(position: Position): T =
    val p = position - offset
    try {
      array(p._2)(p._1)
    } catch {
      case e: IndexOutOfBoundsException =>
        throw new IndexOutOfBoundsException(s"$position does not belong to a rectangle at $offset with size $size")
    }

  def update(position: Position, v: T): Unit =
    val p = position - offset
    array(p._2)(p._1) = v

  def resetOffsetInPlace: Display2D[T] =
    Display2D((0,0), size)(Some(() => array))

  def offsetByInPlace(offset2: Vector2d): Display2D[T] =
    new Display2D[T](offset + offset2, size)(Some(() => array))

  lazy val rect: Rectangle = Rectangle(offset, size)

  def isWithinRange(p: Position): Boolean = rect.isWithinRange(p)
  
  def positionInside(p: Position): Option[Position] = 
    if(isWithinRange(p))
      Some(p)
    else
      None

  def get(position: Position): Option[T] =
    positionInside(position).map(apply)
  /**
    * Updates the position if it is present. Ignores otherwise.
    * This might be helpful if we search for some condition in a given area and don't
    * want to store large array of data.
    */
  def safeUpdate(position: Position, v: T): Boolean =
    val wasUpdated = isWithinRange(position)
    if(wasUpdated) {
      update(position, v)
    }
    wasUpdated
  

  def xs: Range.Exclusive = rect.xs
  def ys: Range.Exclusive = rect.ys
  // initial: () => T = () => {implicitly[Numeric[T]].zero}

  def lineY(y: Int): Array[T] = array(y - minY)

  val minX: Int = offset._1
  val maxXplusExtra1: Int = minX + size._1
  val maxX: Int = maxXplusExtra1 - 1

  val minY: Int = offset._2
  val maxYplusExtra1: Int = minY + size._2
  val maxY: Int = maxYplusExtra1 - 1



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
  def lazyPoints: LazyList[Position] =
    LazyList(points:_*)
  def enlargeBy(n: Int): Display2D[T] =
    val res = new Display2D[T](offset - (n,n), size + (n*2, n*2))()
    for{
      p <- points
    }{
      res(p) = apply(p)
    }
    res

  def shrinkBy(n: Int): Display2D[T] =
    val res = new Display2D[T](offset + (n,n), size - (n*2, n*2))()
    for{
      p <- points
      if res.isWithinRange(p)
    }{
      res(p) = apply(p)
    }
    res

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

  def valuesAround(p: Position): Seq[T] =
    positionsAround(p)
      .map(apply)

//   /** Performs search in the given direction.
//     * If found, returns the position.
//     */
//   @tailrec
//   final def searchInDirection(pos: Position, dir: Direction, pred: T => Boolean): Option[Position] = {
//     val nextPos = pos + dir
//     if(isWithinRange(nextPos)){
//       val c = apply(nextPos)
//       if(pred(c))
//         Some(nextPos)
//       else
//         searchInDirection(nextPos, dir, pred)
//     } else
//       None
//   }

  /** Enumerates all positions on edges.
    * O(N+M)
    * The order is not guaranteed.
    * It might be considered as Set.
    */
  def edges: Seq[Position] =
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
  

  // Up, Left, Down, Right
  def edgePositionsByDirClockwise(dir: Direction): IndexedSeq[Position] = dir match {
    case Geom2dUtils.Up =>
      xs.map((_, minY))
    case Geom2dUtils.Right =>
      ys.map((maxX, _))
    case Geom2dUtils.Left =>
      ys.map((minX, _)).reverse
    case Geom2dUtils.Down =>
      xs.map((_, maxY)).reverse
    case _ =>
      throw new IllegalArgumentException(s"There is no edge at direction $dir")
  }

  /** Sum of all elements in rect inclusive boundaries.
    * Rectangle should be within display boundaries.
    */
  def inclusiveRectSum(topLeft: Position, bottomRight: Position)(using num: Numeric[T]): T = {
    val tl = topLeft - offset
    val br = bottomRight - offset

    @tailrec
    def go(i: Int, j: Int, accum: T): T =
      if (j > br._2)
        accum
      else {
        if (i > br._1)
          go(tl._1, j + 1, accum)
        else
          go(i + 1, j, num.plus(accum, array(i)(j)))
      }

    go(tl._1, tl._2, num.zero)
  }

  //    d.array = array.transpose
  def transpose: Display2D[T] =
    val d = Display2D[T](offset.transpose, size.transpose)()
    for {
      p <- points
      pp = p.transpose
    } {
      d(pp) = apply(p)
    }
    d
  

  def rotateClockwise90: Display2D[T] =
    val d = Display2D[T]((-maxY, minX), size.transpose)()
    for{
      p <- points
      x = p._1
      y = p._2
      pp = (-y, x)
    } {
      d(pp) = apply(p)
    }
    d

  def rotateCounterClockwise90: Display2D[T] =
    val d = Display2D[T]((maxY, -minX), size.transpose)()
    for{
      p <- points
      x = p._1
      y = p._2
      pp = (y, -x)
    } {
      d(pp) = apply(p)
    }
    d
  
  /** Draws the function on this display. */
  def renderFunction(f: Position => T): Unit =
    for{
      p <- points
    } {
      this(p) = f(p)
    }

  def fill(f: Position => T): Unit = renderFunction(f)

  /** Fill display with the given value.
    * Faster than renderFunction(_ => value)
    * */
  def fillAll(value: => T): Unit =
    def newFilledArrayLine = Array.fill(size._1)(value)
    for{
      j <- 0 until size._2
    } {
      array(j) = newFilledArrayLine
    }

  def showDisplay(colWidth: Int = 1)(show: T => String = _.toString): String =
    (for{
      y <- ys
    } yield {
      lineY(y)
        .map(show)
        .map(_.padTo(colWidth, ' ')).mkString
    }).mkString("\n")


  def lazyMap[A: ClassTag](f: T => A): Display2D[A] =
    new Display2D[A](offset, size)(
      Some(
        () => array.map(_.map(f))
      )
    )
  def map[A: ClassTag](f: T => A): Display2D[A] =
    val res = lazyMap(f)
    res.array
    res
  // def map[B: ClassTag](f: T => B): Display2D[B] = {
  //   val a: Array[Array[B]] = array.map(_.map(f).toArray)
  //   new Display2D[B](offset, size)(Some(() => a))
  // }

  type CellularAutomatonRule = (T, Seq[T]) => T

  def produceByRules(relPositions: List[Position])(rules: CellularAutomatonRule): Display2D[T] =
    val d = new Display2D[T](offset, size)()
    for{
      p <- points
      v = relPositions.map(_ + p)
        .filter(isWithinRange)
        .map(apply)
      next = rules(apply(p), v)
    } {
      d(p) = next
    }
    d

  /** Transform this display according to cellular automaton rules. */
  val produceByLocalRules8: CellularAutomatonRule => Display2D[T] = produceByRules(directions8)

  /** Transform this display according to cellular automaton rules. */
  val produceByLocalRulesFromMainDirections: CellularAutomatonRule => Display2D[T] = produceByRules(mainDirections)

  def produceByGlobalRules(rules: ((Int, Int), T) => T): Display2D[T] =
    val d = new Display2D[T](offset, size)()
    for{
      p <- points
      c = apply(p)
      next = rules(p,c)
    } {
      d(p) = next
    }
    d

  def flatten[A: ClassTag](using ev: scala.Conversion[T, Display2D[A]]): Display2D[A] =
    val tl = apply(offset)
    val res = Display2D[A](tl.offset + (offset._1 * tl.size._1, offset._2 * tl.size._2), (tl.size._1 * size._1, tl.size._2 * size._2))()
    res.fill { p =>
      val pp = p - tl.offset
      val x = pp._1 / tl.size._1
      val y = pp._2 / tl.size._2
      val tile = apply((x,y))
      tile((pp._1 % tl.size._1, pp._2 % tl.size._2) + tl.offset)
    }
    res

  def flipY: Display2D[T] =
    val a: Array[Array[T]] = array.reverse
    new Display2D[T]((offset._1, -offset._2), size)(Some(() => a))
  

  case class Pattern(relativePositions: List[Position], value: T):

    def matchesAtPosition(topLeft: Position): Boolean =
      relativePositions
        .forall { rp =>
          val p = topLeft + rp
          isWithinRange(p) &&
            apply(p) == value
        }

    def findAll: LazyList[Position] =
      lazyPoints.filter(matchesAtPosition)

  /** Counts number of elements that satisfy the given predicate. */
  def count(predicate: T => Boolean): Int =
    points.count((apply _).andThen(predicate))

object Display2D:
  def apply[T: ClassTag](rect: Rectangle): Display2D[T] =
    new Display2D[T](rect.topLeft, rect.size)()

  def readCharDisplay(lines: Seq[String], emptyChar: Char = 0): Display2D[Char] =
    val width = lines.map(_.length).max
    Display2D[Char]((0,0), (width, lines.length))(
      Some(
        () => 
          lines.map{ line =>
            val arr = line.toCharArray
            if arr.length == width then
              arr
            else
              val arr2 = Array.fill[Char](width)(emptyChar)
              arr.copyToArray(arr2)
              arr2
          }.toArray
      )
    )
  

  def eq[T](d1: Display2D[T], d2: Display2D[T]): Boolean =
    d1.offset == d2.offset &&
    d1.size == d2.size &&
    d1.array.zip(d2.array)
      .forall{ case (a,b) => a.sameElements(b) }

  def showPoints(points: Seq[Position], pointChar: Char = '.', emptyChar: Char = ' '): Display2D[Char] =
    val rect = Geom2dUtils.boundingRect(points)
    val d = Display2D.apply[Char](rect)
    d.fillAll(emptyChar)
    points.foreach(p => d(p) = pointChar)
    d

  def of[T: ClassTag](offset: Vector2d, size: Vector2d)(f: Position => T): Display2D[T] =
    val d = Display2D[T](offset, size)()
    d.fill(f)
    d
