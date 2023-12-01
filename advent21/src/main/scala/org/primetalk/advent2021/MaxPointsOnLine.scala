package org.primetalk.advent2021

/**
 * Given an array of points where points[i] = [xi, yi] represents a point on the X-Y plane, 
 * return the maximum number of points that lie on the same straight line.
 */
object MaxPointsOnLine {
  type Point = Array[Int]
  case class Line(p1: Point, p2: Point) {
    val Array(x1, y1) = p1
    val Array(x2, y2) = p2
    def belongs(p: Point): Boolean = {
      val Array(x, y) = p
      (x - x1) * (y2 - y1) == (y - y1) * (x2 - x1)
    }
  }
  def maxPoints(points: Array[Array[Int]]): Int = {
    def linesWithP1(p1: Point, otherPoints: List[Point], max: Int = 1): Int = otherPoints match {
      case Nil => 
        max
      case p2 :: t => 
        val line = Line(p1, p2)
        val (onLine, rest) = t.partition(line.belongs)
        val cnt = onLine.size + 2
        linesWithP1(p1, rest, math.max(max, cnt))
    }
    def maxPoints0(points: List[Point], max: Int = 1): Int = points match {
      case Nil => 
        max
      case p1 :: t =>
        val m = linesWithP1(p1, t)
        maxPoints0(t, math.max(max, m))
    }
    maxPoints0(points.toList)
  }
}
