package org.primetalk.advent3.tools

import javax.swing.text.Position
import scala.reflect.ClassTag
import scala.util.Random

object Geom3dLongUtils:
  type Vector3d = (Long, Long, Long)
  type Position = Vector3d
  type Direction = Vector3d

  type InclusiveRange = (Long, Long) // [min,max]

  extension (r: InclusiveRange)

    def isInside(x: Long): Boolean =
      r._1 <= x && x <= r._2

    def intersect(other: InclusiveRange): Option[InclusiveRange] =
      if other._1 < r._1 then
        other.intersect(r)
      else
        // other._1 >= r._1
        if other._1 <= r._2 then
          if isInside(other._2) then
            Some(other)
          else
            Some((other._1, r._2))
        else
          None

    /** 0..2 elements in the result. */
    def exclude(o: InclusiveRange): List[InclusiveRange] =
      List(
        if r._1 < o._1 then
          List((r._1, math.min(o._1 - 1, r._2)))
        else
          Nil
        ,
        if o._2 < r._2 then
          List((math.max(r._1, o._2 + 1), r._2))
        else
          Nil
      )
        .flatten
        .distinct


  def parse3d(line: String): Position =
    val Seq(x,y,z) = Utils.parseAllLongsInString(line)
    (x,y,z):Position

  def parallelepipedByDiagonal(topLeft: Position, bottomRight: Position): Parallelepiped =
    Parallelepiped(topLeft, bottomRight - topLeft + (1, 1, 1))

  def parallelepipedFromRanges(xr: InclusiveRange, yr: InclusiveRange, zr: InclusiveRange): Parallelepiped =
    parallelepipedByDiagonal((xr._1, yr._1, zr._1), (xr._2, yr._2, zr._2))

  // sealed trait OrtPlain
  // case class OrtPlainX(x: Long) extends OrtPlain
  // case class OrtPlainY(y: Long) extends OrtPlain
  // case class OrtPlainZ(z: Long) extends OrtPlain

  case class Line3d(a: Position, b: Position)

  /** Origin is in top left corner. */
  case class Parallelepiped(topLeft: Position, size: Vector3d):

    def bottomRight: Position = topLeft + size - (1, 1, 1)

    def containsCompletely(other: Parallelepiped): Boolean =
      isWithin(other.topLeft) &&
        isWithin(other.bottomRight) 

    def hasIntersection(other: Parallelepiped): Boolean =
      intersect(other).nonEmpty
    def isWithin(p: Position): Boolean = p match
      case (x,y,z) =>
        x0 <= x && (x - x0) < size._1 &&
        y0 <= y && (y - y0) < size._2 &&
        z0 <= z && (z - z0) < size._3

    def isOnBoundary(p: Position): Boolean = p match
      case (x,y,z) =>
        x0 == x || x1 == x ||
        y0 == y || y1 == y ||
        z0 == z || z1 == z 

    def x0: Long = topLeft._1
    def y0: Long = topLeft._2
    def z0: Long = topLeft._3

    def x1: Long = x0 + size._1 - 1
    def y1: Long = y0 + size._2 - 1
    def z1: Long = z0 + size._3 - 1

    def xr: InclusiveRange = (x0, x1)
    def yr: InclusiveRange = (y0, y1)
    def zr: InclusiveRange = (z0, z1)

    def isValid: Boolean =
      size.isAllPositive

    def randomPoint(rnd: Random): Position =
      val s = size
      val i = rnd.nextLong(s._1)
      val j = rnd.nextLong(s._2)
      val k = rnd.nextLong(s._3)
      (i, j, k) + topLeft

    def vertices: List[Position] =
      List(
        (x0,y0,z0),
        (x0,y0,z1),
        (x0,y1,z0),
        (x0,y1,z1),
        (x1,y0,z0),
        (x1,y0,z1),
        (x1,y1,z0),
        (x1,y1,z1)
      )

    def edges: List[Line3d] =
      val v = vertices
      for
        a <- v
        b <- v
        if a != b
        if a._1 == b._1 || a._2 == b._2 || a._3 == b._3 
      yield 
        Line3d(a,b)
    def faces: List[Parallelepiped] = 
      List(
        parallelepipedByDiagonal((x0,y0,z0), (x0,y1,z1)),
        parallelepipedByDiagonal((x0,y0,z0), (x1,y0,z1)),
        parallelepipedByDiagonal((x0,y0,z0), (x1,y1,z0)),
        parallelepipedByDiagonal((x1,y1,z1), (x1,y0,z0)),
        parallelepipedByDiagonal((x1,y1,z1), (x0,y1,z0)),
        parallelepipedByDiagonal((x1,y1,z1), (x0,y0,z1)),
      )
      
    def manhattanSize: Long = size.manhattanSize

    def coordinatePoints: Seq[Position] = Seq(topLeft, bottomRight)

    def points: Seq[Position] =
      for
        z <- z0 to z1
        y <- y0 to y1
        x <- x0 to x1
      yield
        (x,y,z)

    def volume: BigInt =
      BigInt(size._1) * size._2 * size._3

    def intersect(other: Parallelepiped): Option[Parallelepiped] =
      for
        rxi <- xr.intersect(other.xr)
        ryi <- yr.intersect(other.yr)
        rzi <- zr.intersect(other.zr)
      yield
        parallelepipedFromRanges(rxi, ryi, rzi)

    /** From the set of points represented by this parallelepiped, remove another one. 
     * If there is no intersection, this one is returned.
     * If there is an intersection, 
     * a collection of parallelepipeds is returned. Combining them together and 
     * the intersection will recreate volume of the original parallelepiped.
     */
    def subtract(other: Parallelepiped): List[Parallelepiped] =
      intersect(other) match
        case None => 
          List(this)
        case Some(i) =>
          val ixr = i.xr
          val iyr = i.yr
          val izr = i.zr

          val gxr = xr.exclude(ixr)
          val gyr = yr.exclude(iyr)
          val gzr = zr.exclude(izr)

          for
            zrr <- izr :: gzr
            yrr <- iyr :: gyr
            xrr <- ixr :: gxr
            if xrr != ixr || yrr != iyr || zrr != izr
          yield
            parallelepipedFromRanges(xrr, yrr, zrr)

    def divideIntoSmallerPieces(n: Long): Seq[Parallelepiped] = {
      if(size == (1,1,1))
        Seq(this)
      else
        (for {
          i <- 0L until n
          j <- 0L until n
          k <- 0L until n
          tl = (x0 + size._1 * i    / n, y0 + size._2 * j      / n, z0 + size._3 * k     / n)
          br = (x0 + size._1 * (i+1)/ n, y0 + size._2 * (j + 1)/ n, z0 + size._3 *(k + 1)/ n) - (1, 1, 1)
        } yield
          parallelepipedByDiagonal(tl, br))
            .filter(_.isValid)
            .filterNot(_ == this)
            .distinct
    }
    def movedBy(shift: Vector3d): Parallelepiped =
      Parallelepiped(topLeft + shift, size)
  end Parallelepiped

  type ManhattanPosition = Position
  type ManhattanParallelepiped = Parallelepiped
  /**
    * Manhattan affine transform is applied - rotate +45°, +45° and scale by sqrt(3)/3.
    * unit coordinate vectors are:
    *
    * i = ( 1, 1,-1)
    * j = ( 1,-1,-1)
    * k = ( 1,-1, 1)
    * (all have length sqrt(3) and are linearly independent)
    * NB! Basis vectors are not orthogonal!
    * These vectors are normal to all manhattan equidistant planes.
    */
  def  manhattanTransform(p: Position): ManhattanPosition = {
    (p._1 + p._2 - p._3, p._1 - p._2 - p._3, p._1 - p._2 + p._3)
  }

  def fromManhattan(p: ManhattanPosition): Position = {
    ((p._1 + p._3)/2 , (p._1 - p._2)/2, (p._3 - p._2)/2)
  }

  def manhattanSphere(p: Position, r: Long): ManhattanParallelepiped = {
    parallelepipedByDiagonal(manhattanTransform(p - (r,0,0)), manhattanTransform(p + (r,0,0)))
  }

  /** Finds bounding rectangle for a collection of points. */
  def boundingParallelepiped(positions: Seq[Position]): Parallelepiped =
    val xs = positions.map(_._1)
    val ys = positions.map(_._2)
    val zs = positions.map(_._3)
    parallelepipedByDiagonal(
      topLeft = (xs.min, ys.min, zs.min),
      bottomRight = (xs.max, ys.max, zs.max)
    )

  def meanAndStdDist(positions: Seq[Position]): (Position, Vector3d) =
    val count = positions.size
    val xs = positions.map(_._1)
    val ys = positions.map(_._2)
    val zs = positions.map(_._2)
    val mean = (xs.sum/count, ys.sum/count, zs.sum/count)
    def stdDevCalc(els: Seq[Long], mean: Long): Long =
      math.sqrt(els.map(x => (x - mean) * (x - mean) ).sum/count).toLong

    val stdDev = (stdDevCalc(xs, mean._1), stdDevCalc(ys, mean._2), stdDevCalc(zs, mean._3))
    (mean, stdDev)

  val origin: Position = (0, 0, 0)

  val directions26: List[Position] =
    (
      for
        x <- -1L to 1
        y <- -1L to 1
        z <- -1L to 1
        if x!=0 || y != 0 || z != 0
      yield 
        (x,y,z)
      ).toList

  assert(directions26.size == 26)

  val mainDirections6: List[Position] = 
    (
      for
        x <- -1L to 1
        y <- -1L to 1
        z <- -1L to 1
        if (x!=0 || y != 0 || z != 0) && (x * y == 0 && y * z == 0 && x * z == 0)
      yield 
        (x,y,z)
      ).toList

  assert(mainDirections6.size == 6)

  extension (p: Position)
    def +(vector: Vector3d): Position =
      (p._1 + vector._1, p._2 + vector._2, p._3 + vector._3)
    def -(vector: Vector3d): Position =
      (p._1 - vector._1, p._2 - vector._2, p._3 - vector._3)

    def unary_- : Position =
      (-p._1, -p._2, -p._3)  
    def isAllNonNegative: Boolean =
      p._1 >= 0 && p._2 >= 0 && p._3 >= 0

    def isAllPositive: Boolean =
      p._1 >0 && p._2 >0 && p._3 >0

    def size: Long = (p._1 * p._1) + (p._2 * p._2) + (p._3 * p._3)
    def length2: Long = (p._1 * p._1) + (p._2 * p._2) + (p._3 * p._3)
    def manhattanSize: Long = p._1.abs + p._2.abs + p._3.abs

    def x = p._1

    def y = p._2
  
    def z = p._3

  val ii = (1, 0, 0)
  val jj = (0, 1, 0)
  val kk = (0, 0, 1)

  extension (a: Vector3d)

    def *(k: Long): Vector3d = a match
      case (x, y, z) => (x * k, y * k, z * k)

    def /(k: Long): Vector3d = a match
      case (x, y, z) => (x / k, y / k, z / k)

    def **(b: Vector3d): Vector3d = 
      (a._2 * b._3 - a._3 * b._2, a._3* b._1 - a._1 * b._3, a._1 * b._2 - a._2 * b._1)

  def manhattanDistance(v1: Vector3d, v2: Vector3d): Long =
    (v1._1 - v2._1).abs +
      (v1._2 - v2._2).abs +
      (v1._3 - v2._3).abs

  def normalizeDirVector(v: Vector3d): Vector3d = 
    v match
      case (x, y, z) =>
        (
          if(x == 0) 0 else x/math.abs(x),
          if(y == 0) 0 else y/math.abs(y),
          if(z == 0) 0 else z/math.abs(z),
        )

  type Matrix4 = IArray[IArray[Long]]

  type TMatrix4 = 
    (
      (Long, Long, Long, Long),
      (Long, Long, Long, Long),
      (Long, Long, Long, Long),
      (Long, Long, Long, Long)
    )
  def Matrix4(t: TMatrix4): Matrix4 = 
      IArray(
        IArray.unsafeFromArray(t._1.toList.toArray[Long]),
        IArray.unsafeFromArray(t._2.toList.toArray[Long]),
        IArray.unsafeFromArray(t._3.toList.toArray[Long]),
        IArray.unsafeFromArray(t._4.toList.toArray[Long])
      )

  extension (a: IArray[Long])
   
    def *(b: IArray[Long]): Long =
      a.zip(b).map(p => p._1*p._2).sum
    
    def toTuple4: (Long, Long, Long, Long) =
      (a(0), a(1), a(2), a(3))

  extension [T](t: (T, T, T, T))
    def toIArray4(using ClassTag[T]): IArray[T] =
      IArray(t(0), t(1), t(2), t(3))

  extension (a: TMatrix4)
    def toMatrix4: Matrix4 =
      IArray(
        a(0).toIArray4,
        a(1).toIArray4,
        a(2).toIArray4,
        a(3).toIArray4
      )
    def *(v: Vector3d): Vector3d =
      val av = a.toMatrix4 * IArray(v._1, v._2, v._3, 1)
      (av(0), av(1), av(2))

  extension (a: Matrix4)
    def toTMatrix: TMatrix4 =
      (
      (a(0)(0), a(1)(0), a(2)(0),a(3)(0)),
      (a(0)(1), a(1)(1), a(2)(1),a(3)(1)),
      (a(0)(2), a(1)(2), a(2)(2),a(3)(2)),
      (a(0)(3), a(1)(3), a(2)(3),a(3)(3))
      )
    def transpose: Matrix4 =
      IArray.unsafeFromArray(
        (
          for
            j <- 0 to 3
          yield
            IArray.unsafeFromArray((for 
              i <- 0 to 3
            yield
              a(i)(j)
              ).toArray
            )
        ).toArray
      )
     
    def *(b: Matrix4): Matrix4 = 
      val bb = b.transpose
      IArray.unsafeFromArray(
        (for
          i <- 0 to 3
        yield
          IArray.unsafeFromArray(
          (for 
            j <- 0 to 3
          yield
            a(i) * bb(j)
            ).toArray
          )
        ).toArray
      )

    def *(v: IArray[Long]): IArray[Long] =
      IArray.unsafeFromArray(
         (for
          i <- 0 to 3
        yield
          a(i) * v).toArray
      )
    def show: String = 
      a.map(_.mkString(",")).mkString("\n")

  type RotationIndex = 0 | 1 | 2 | 3

  extension (ri: RotationIndex)
    def cos: Long =
      Array(1, 0, -1, 0)(ri)
    def sin: Long =
      Array(0, 1, 0, -1)(ri)

  def rx(ri: RotationIndex): Matrix4 =
    Matrix4 (
      (1, 0,      0,       0),
      (0, ri.cos, -ri.sin, 0),
      (0, ri.sin,  ri.cos, 0),
      (0, 0,      0,       1)
      )
  def ry(ri: RotationIndex): Matrix4 =
    Matrix4 (
      ( ri.cos, 0,  ri.sin, 0),
      (0,       1,  0,      0),
      (-ri.sin, 0,  ri.cos, 0),
      (0,       0,  0,      1)
      )
  def rz(ri: RotationIndex): Matrix4 =
    Matrix4(
      ( ri.cos,-ri.sin, 0, 0),
      ( ri.sin, ri.cos, 0, 0),
      (0,       0,      1, 0),
      (0,       0,      0, 1)
      )
  
  val rotationIndices: Seq[RotationIndex] = Seq(0, 1, 2, 3)

  def rotations: Set[TMatrix4] =
    (for
      a <- rotationIndices
      b <- rotationIndices
      c <- rotationIndices
    yield
      (rx(a) * ry(b) * rz(c)).toTMatrix
    ).toSet

  assert(rotations.size == 24, s"size = ${rotations.size}")
  val noRotation = (rx(0) * ry(0) * rz(0)).toTMatrix

  type Quaternion = (Long, Long, Long, Long)

  // /** rotIndex: 
  //   0 - 0
  //  *1 - 90
  //  *2 - 180
  //  *3 - 270 
  //  */
  // def rotationQuaternion(dir: Vector3d, rotIndex: RotationIndex): Quaternion = ???

  /** Represents solid body orientation. Has a single direction vector (one of 6 main directions)
   * and rotation around that direction.
   */
  case class BodyOrientation(dir: Vector3d, rot: RotationIndex)
