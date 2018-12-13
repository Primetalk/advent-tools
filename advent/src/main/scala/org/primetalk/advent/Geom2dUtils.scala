package org.primetalk.advent

object Geom2dUtils {
  type Position = (Int, Int)
  type Vector = (Int, Int)
  type Direction = Vector
  /** Origin is in top left corner. */
  case class Rectangle(topLeft: Position, bottomRight: Position) {
    def area: Long = {(bottomRight._1 - topLeft._1) * (bottomRight._2 - topLeft._2)}
  }

  val origin: Position = (0, 0)

  val Up: Direction = (0, +1)
  val Down: Direction = (0, -1)
  val Left: Direction = (-1, 0)
  val Right: Direction = (1, 0)

  lazy val mainDirections: Seq[Direction] = Seq(Up, Left, Down, Right)
  lazy val directions8: Seq[Direction] = mainDirections ++ Seq(Up + Right, Up + Left, Down + Left, Down + Right)

  def mul(k: Int): Direction => Vector = {
    case (x, y) => (x * k, y * k)
  }

  implicit class PosOps(p: Position) {
    def +(vector: Vector): Position =
      (p._1 + vector._1, p._2 + vector._2)
    def -(vector: Vector): Position =
      (p._1 - vector._1, p._2 - vector._2)
  }

  implicit class VecOps(v: Vector) {

    def *(k: Int): Vector = v match {
      case (x, y) => (x * k, y * k)
    }

    def transpose: Vector =
      (v._2, v._1)
  }

  def manhattanDistance(p1: Position, p2: Position): Int = {
    math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)
  }

}
