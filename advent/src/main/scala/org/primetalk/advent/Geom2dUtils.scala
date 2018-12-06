package org.primetalk.advent

object Geom2dUtils {
  type Position = (Int, Int)
  type Vector = (Int, Int)
  type Direction = Vector

  val Up: Direction = (0, +1)
  val Down: Direction = (0, -1)
  val Left: Direction = (-1, 0)
  val Right: Direction = (1, 0)

  def mul(k: Int): Direction => Vector = {
    case (x, y) => (x * k, y * k)
  }

  implicit class PosOps(p: Position) {
    def +(vector: Vector): Position =
      (p._1 + vector._1, p._2 + vector._2)
    def -(vector: Vector): Position =
      (p._1 - vector._1, p._2 - vector._2)
  }

  val origin: Position = (0, 0)

  def manhattanDistance(p1: Position, p2: Position): Int = {
    math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)
  }

}
