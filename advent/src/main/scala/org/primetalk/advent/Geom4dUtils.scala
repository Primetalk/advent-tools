package org.primetalk.advent

object Geom4dUtils {
  type Vector4d = (Int, Int, Int, Int)

  val origin: Vector4d = (0, 0, 0, 0)

  implicit class PosOps(p: Vector4d) {
    def +(vector: Vector4d): Vector4d =
      (p._1 + vector._1, p._2 + vector._2, p._3 + vector._3, p._4 + vector._4)
    def -(vector: Vector4d): Vector4d =
      (p._1 - vector._1, p._2 - vector._2, p._3 - vector._3, p._4 - vector._4)

    def size: Long = (p._1 * p._1) + (p._2 * p._2) + (p._3 * p._3) + (p._4 * p._4)

    def manhattanSize: Long = p._1.abs + p._2.abs + p._3.abs + p._4.abs
  }

  implicit class VecOps(v: Vector4d) {

    def *(k: Int): Vector4d = v match {
      case (x, y, z, xx) => (x * k, y * k, z * k, xx * k)
    }

    def /(k: Int): Vector4d = v match {
      case (x, y, z, xx) => (x / k, y / k, z / k, xx / k)
    }
  }

  def manhattanDistance(v1: Vector4d, v2: Vector4d): Int = {
    (v1._1 - v2._1).abs +
      (v1._2 - v2._2).abs +
      (v1._3 - v2._3).abs +
      (v1._4 - v2._4).abs
  }

}
