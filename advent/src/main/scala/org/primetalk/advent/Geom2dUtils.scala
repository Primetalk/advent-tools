package org.primetalk.advent

object Geom2dUtils {
  type Position = (Int, Int)
  type Vector2d = (Int, Int)
  type Direction = Vector2d

  def rectangleByDiagonal(topLeft: Position, bottomRight: Position): Rectangle = {
    Rectangle(topLeft, bottomRight - topLeft + (1, 1))
  }

  /** Origin is in top left corner. */
  case class Rectangle(topLeft: Position, size: Vector2d) {

    def area: Long =
      size._1 * size._2

    def coordinatePoints: Seq[Position] = Seq(topLeft, bottomRight)

    def bottomRight: Position = topLeft + size - (1, 1)
  }

  /** Finds bounding rectangle for a collection of points. */
  def boundingRect(positions: Seq[Position]): Rectangle = {
    val xs = positions.map(_._1)
    val ys = positions.map(_._2)
    rectangleByDiagonal(
      topLeft = (xs.min, ys.min),
      bottomRight = (xs.max, ys.max)
    )
  }

  val origin: Position = (0, 0)

  val Up: Direction = (0, +1)
  val Down: Direction = (0, -1)
  val Left: Direction = (-1, 0)
  val Right: Direction = (1, 0)

  lazy val mainDirections: Seq[Direction] = Seq(Up, Left, Down, Right)
  lazy val mainDirectionsInReadingOrder: Seq[Direction] = Seq(Up, Left, Right, Down)
  lazy val directions8: Seq[Direction] = mainDirections ++ Seq(Up + Right, Up + Left, Down + Left, Down + Right)

  def mul(k: Int): Direction => Vector2d = {
    case (x, y) => (x * k, y * k)
  }

  implicit class PosOps(p: Position) {
    def +(vector: Vector2d): Position =
      (p._1 + vector._1, p._2 + vector._2)
    def -(vector: Vector2d): Position =
      (p._1 - vector._1, p._2 - vector._2)
  }

  implicit class VecOps(v: Vector2d) {

    def *(k: Int): Vector2d = v match {
      case (x, y) => (x * k, y * k)
    }

    def transpose: Vector2d =
      (v._2, v._1)

    /** Rotates as a multiplication of complex numbers. */
    def rotate(o: Vector2d): Vector2d =
      (v._1 * o._1 - v._2 * o._2, v._1 * o._2 + v._2 * o._1)
  }

  def manhattanDistance(p1: Position, p2: Position): Int = {
    math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)
  }

  val readingOrdering: Ordering[Position] =
    (a: (Int, Int), b: (Int, Int)) => {
      val cmp1 = a._2 - b._2
      if(cmp1 != 0)
        cmp1
      else
        a._1 - b._1
    }
  type ManhattanPosition = (Int, Int)

  /**
    * Manhattan affine transform is applied - rotate +45° and scale by sqrt(2)/2.
    */
  def manhattanTransform(p: Position): ManhattanPosition = {
    (p._2 + p._1, p._2 - p._1)
  }
  /** It's a rectangle that is constructed by diagonals.
    * For this an affine transform is applied - rotate +45° and scale by sqrt(2)/2.
    * It's also enough to describe it with just two points.
    */
  case class ManhattanEllipse(p1: Position, p2: Position) {
    def size: Vector2d =
      manhattanTransform(p2 - p1)
  }

  def manhattanCircle(p: Position, r: Int): ManhattanEllipse = {
    ManhattanEllipse(p - (r,0), p + (r,0))
  }

}
