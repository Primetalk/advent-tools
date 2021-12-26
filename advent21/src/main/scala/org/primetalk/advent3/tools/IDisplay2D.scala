package org.primetalk.advent3.tools

import org.primetalk.advent3.tools.Geom2dUtils.*

import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
  * Immutable Display is oriented (x: left->right, y: top->down.).
  *
  * @param offset - top-left corner. ys increase down, xs increase right
  */
// TODO: DisplayView - rotation on n*90; shift; constrain size; flip up/down
// DONE showDisplay
// TODO: DrawDisplay on canvas (Scala.js)
// TODO: Remove state. Mutable array could be provided from outside as an implicit context
// TODO: Use refined type for array size,vector size.
final case class IDisplay2D[T: ClassTag](offset: Vector2d, size: Vector2d)(init: Option[() => IArray[IArray[T]]] = None):
  /** We only allocate array when it's needed*/
  lazy val array: IArray[IArray[T]] =
    init.getOrElse(() => IArray.unsafeFromArray(Array.ofDim[T](size._2, size._1).map(IArray.unsafeFromArray))).apply()

  def apply(position: Position): T =
    val p = position - offset
    try {
      array(p._2)(p._1)
    } catch {
      case e: IndexOutOfBoundsException =>
        throw new IndexOutOfBoundsException(s"$position does not belong to a rectangle at $offset with size $size")
    }

  def updated(position: Position, v: T): IDisplay2D[T] =
    val p = position - offset
    IDisplay2D(offset, size)(Some(() => array.updated(p._2, array(p._2).updated(p._1, v))))

  def resetOffsetInPlace: IDisplay2D[T] =
    IDisplay2D((0,0), size)(Some(() => array))

  def offsetByInPlace(offset2: Vector2d): IDisplay2D[T] =
    new IDisplay2D[T](offset + offset2, size)(Some(() => array))

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
  def safeUpdated(position: Position, v: T): IDisplay2D[T] =
    val wasUpdated = isWithinRange(position)
    if wasUpdated then
      updated(position, v)
    else 
      this  

  def xs: Range.Exclusive = rect.xs
  def ys: Range.Exclusive = rect.ys
  // initial: () => T = () => {implicitly[Numeric[T]].zero}

  def lineY(y: Int): IArray[T] = array(y - minY)

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
  def enlargeBy(n: Int, empty: T): IDisplay2D[T] =
    IDisplay2D.fromFunction(Rectangle(offset - (n,n), size + (n*2, n*2))){ 
      pos => 
        if this.isWithinRange(pos) then this(pos) else empty
    }


  def shrinkBy(n: Int): IDisplay2D[T] =
    IDisplay2D.fromFunction(Rectangle(offset + (n,n), size - (n*2, n*2)))(this.apply)

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
  def transpose: IDisplay2D[T] =
    IDisplay2D.fromFunction(Rectangle(offset.transpose, size.transpose))(p => this((p._2, p._1)))

  def rotateClockwise90: IDisplay2D[T] =
    IDisplay2D.fromFunction(Rectangle((-maxY, minX), size.transpose))(p => this((-p._2, p._1)))

  def rotateCounterClockwise90: IDisplay2D[T] =
    IDisplay2D.fromFunction(Rectangle((maxY, -minX), size.transpose))(p => this((p._2, -p._1)))
  
  /** Draws the function on this display. */
  def renderFunction(f: Position => T): IDisplay2D[T] =
    IDisplay2D.fromFunction(rect)(f)

  val fill = renderFunction

  /** Fill display with the given value.
    * Faster than renderFunction(_ => value)
    * */
  def fillAll(value: => T): IDisplay2D[T] =
    IDisplay2D.fillAll(rect)(value)

  def showDisplay(colWidth: Int = 1)(show: T => String = _.toString): String =
    (for{
      y <- ys
    } yield {
      lineY(y)
        .map(show)
        .map(_.padTo(colWidth, ' ')).mkString
    }).mkString("\n")

  def lazyMap[A: ClassTag](f: T => A): IDisplay2D[A] =
    IDisplay2D.fromFunction(rect)(this.apply andThen f)
    
  def map[A: ClassTag](f: T => A): IDisplay2D[A] =
    val res = lazyMap(f)
    res.array
    res

  type CellularAutomatonRule = (T, Seq[T]) => T

  def produceByRules(relPositions: List[Position])(rules: CellularAutomatonRule): IDisplay2D[T] =
    IDisplay2D.fromFunction(rect)(p => 
      val v = relPositions.map(_ + p)
        .filter(isWithinRange)
        .map(apply)
      rules(apply(p), v)
    )

  /** Transform this display according to cellular automaton rules. */
  val produceByLocalRules8: CellularAutomatonRule => IDisplay2D[T] = produceByRules(directions8)

  val neibourhood9 = (0,0) :: directions8
  
  /** Transform this display according to cellular automaton rules. */
  val produceByLocalRules9: CellularAutomatonRule => IDisplay2D[T] = produceByRules(neibourhood9)

  /** Transform this display according to cellular automaton rules. */
  val produceByLocalRulesFromMainDirections: CellularAutomatonRule => IDisplay2D[T] = produceByRules(mainDirections)

  def produceByGlobalRules(rules: ((Int, Int), T) => T): IDisplay2D[T] =
    IDisplay2D.fromFunction(rect)(p => 
      val c = apply(p)
      rules(p,c)
    )
  def replace(old: T, n: T): IDisplay2D[T] =
    produceByGlobalRules{ case (_, v) => if v == old then n else v }
  def flatten[A: ClassTag](using ev: scala.Conversion[T, IDisplay2D[A]]): IDisplay2D[A] =
    val tl = apply(offset)
    IDisplay2D.fromFunction(Rectangle(tl.offset + (offset._1 * tl.size._1, offset._2 * tl.size._2), (tl.size._1 * size._1, tl.size._2 * size._2))){ p => 
      val pp = p - tl.offset
      val x = pp._1 / tl.size._1
      val y = pp._2 / tl.size._2
      val tile = apply((x,y))
      tile((pp._1 % tl.size._1, pp._2 % tl.size._2) + tl.offset)
    }

  def flipY: IDisplay2D[T] =
    IDisplay2D[T]((offset._1, -offset._2), size)(Some(() => array.reverse))

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
  def findAll(p: T => Boolean): Seq[Position] =
    points.filter(pos => p(apply(pos)))

  def linePositions(y: Int): Seq[Position] =
    xs.map(x => (x, y)).toSeq
  def columnPositions(x: Int): Seq[Position] =
    ys.map(y => (x, y)).toSeq

  def deepClone: IDisplay2D[T] = 
    val res = IDisplay2D.apply[T](rect)
    res.fill(this.apply)
    res

  def isEqualTo(other: IDisplay2D[T]): Boolean =
    offset == other.offset &&
    size == other.size &&
    array.zip(other.array).forall(_.sameElements(_))

  def elementClassTag: ClassTag[T] = 
    summon[ClassTag[T]]

  override def equals(other: Any): Boolean =
    other.isInstanceOf[IDisplay2D[_]] &&
      other.asInstanceOf[IDisplay2D[_]].elementClassTag == elementClassTag &&
      this.isEqualTo(other.asInstanceOf[IDisplay2D[T]])

object IDisplay2D:
  def apply[T: ClassTag](rect: Rectangle): IDisplay2D[T] =
    new IDisplay2D[T](rect.topLeft, rect.size)()

  def of[T: ClassTag](offset: Vector2d, size: Vector2d)(f: Position => T): IDisplay2D[T] =
    Display2D.of(offset, size)(f).toIDisplay2D

  def fromFunction[T: ClassTag](rect: Rectangle)(f: Position => T): IDisplay2D[T] =
    of(rect.topLeft, rect.size)(f)

  /** Fill display with the given value.
    * Faster than renderFunction(_ => value)
    * */
  def fillAll[T: ClassTag](rect: Rectangle)(value: => T): IDisplay2D[T] =
    new IDisplay2D[T](rect.topLeft, rect.size)(Some(() =>
      val display = Display2D.apply(rect)
      display.fillAll(value)
      IArray.unsafeFromArray(display.array.map(IArray.unsafeFromArray))
    ))

  def readCharDisplay(lines: Seq[String], emptyChar: Char = 0): IDisplay2D[Char] =
    val width = lines.map(_.length).max
    IDisplay2D[Char]((0,0), (width, lines.length))(
      Some(
        () => 
          IArray.unsafeFromArray(
            lines.map{ line =>
              val arr = line.toCharArray
              IArray.unsafeFromArray(
                if arr.length == width then
                  arr
                else
                  val arr2 = Array.fill[Char](width)(emptyChar)
                  arr.copyToArray(arr2)
                  arr2
              )
            }.toArray
          )
      )
    )
  

  def eq[T](d1: IDisplay2D[T], d2: IDisplay2D[T]): Boolean =
    d1.offset == d2.offset &&
    d1.size == d2.size &&
    d1.array.zip(d2.array)
      .forall{ case (a,b) => a.sameElements(b) }

  def showPoints(points: Set[Position], pointChar: Char = '.', emptyChar: Char = ' '): IDisplay2D[Char] =
    val rect = Geom2dUtils.boundingRect(points)
    IDisplay2D.fromFunction[Char](rect){ p => 
      if points.contains(p) then 
        pointChar 
      else 
        emptyChar
    }
