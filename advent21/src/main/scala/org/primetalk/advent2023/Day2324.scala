package org.primetalk.advent2023

import org.primetalk.advent3.tools.Utils
import org.primetalk.advent3.tools.Geom3dLongUtils.{given,*}
/**
  * https://adventofcode.com/2023/day/24

Our sponsors help make Advent of Code possible:
Railway - Instant deployments, effortless scale.
--- Day 24: Never Tell Me The Odds ---

It seems like something is going wrong with the snow-making process. Instead of forming snow, the water that's been absorbed into the air seems to be forming hail!

Maybe there's something you can do to break up the hailstones?

Due to strong, probably-magical winds, the hailstones are all flying through the air in perfectly linear trajectories. You make a note of each hailstone's position and velocity (your puzzle input). For example:

19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3

Each line of text corresponds to the position and velocity of a single hailstone. The positions indicate where the hailstones are right now (at time 0). The velocities are constant and indicate exactly how far each hailstone will move in one nanosecond.

Each line of text uses the format px py pz @ vx vy vz. For instance, the hailstone specified by 20, 19, 15 @ 1, -5, -3 has initial X position 20, Y position 19, Z position 15, X velocity 1, Y velocity -5, and Z velocity -3. After one nanosecond, the hailstone would be at 21, 14, 12.

Perhaps you won't have to do anything. How likely are the hailstones to collide with each other and smash into tiny ice crystals?

To estimate this, consider only the X and Y axes; ignore the Z axis. Looking forward in time, how many of the hailstones' paths will intersect within a test area? (The hailstones themselves don't have to collide, just test for intersections between the paths they will trace.)

In this example, look for intersections that happen with an X and Y position each at least 7 and at most 27; in your actual data, you'll need to check a much larger test area. Comparing all pairs of hailstones' future paths produces the following results:

Hailstone A: 19, 13, 30 @ -2, 1, -2
Hailstone B: 18, 19, 22 @ -1, -1, -2
Hailstones' paths will cross inside the test area (at x=14.333, y=15.333).

Hailstone A: 19, 13, 30 @ -2, 1, -2
Hailstone B: 20, 25, 34 @ -2, -2, -4
Hailstones' paths will cross inside the test area (at x=11.667, y=16.667).

Hailstone A: 19, 13, 30 @ -2, 1, -2
Hailstone B: 12, 31, 28 @ -1, -2, -1
Hailstones' paths will cross outside the test area (at x=6.2, y=19.4).

Hailstone A: 19, 13, 30 @ -2, 1, -2
Hailstone B: 20, 19, 15 @ 1, -5, -3
Hailstones' paths crossed in the past for hailstone A.

Hailstone A: 18, 19, 22 @ -1, -1, -2
Hailstone B: 20, 25, 34 @ -2, -2, -4
Hailstones' paths are parallel; they never intersect.

Hailstone A: 18, 19, 22 @ -1, -1, -2
Hailstone B: 12, 31, 28 @ -1, -2, -1
Hailstones' paths will cross outside the test area (at x=-6, y=-5).

Hailstone A: 18, 19, 22 @ -1, -1, -2
Hailstone B: 20, 19, 15 @ 1, -5, -3
Hailstones' paths crossed in the past for both hailstones.

Hailstone A: 20, 25, 34 @ -2, -2, -4
Hailstone B: 12, 31, 28 @ -1, -2, -1
Hailstones' paths will cross outside the test area (at x=-2, y=3).

Hailstone A: 20, 25, 34 @ -2, -2, -4
Hailstone B: 20, 19, 15 @ 1, -5, -3
Hailstones' paths crossed in the past for hailstone B.

Hailstone A: 12, 31, 28 @ -1, -2, -1
Hailstone B: 20, 19, 15 @ 1, -5, -3
Hailstones' paths crossed in the past for both hailstones.

So, in this example, 2 hailstones' future paths cross inside the boundaries of the test area.

However, you'll need to search a much larger test area if you want to see if any hailstones might collide. Look for intersections that happen with an X and Y position each at least 200000000000000 and at most 400000000000000. Disregard the Z axis entirely.

Considering only the X and Y axes, check all pairs of hailstones' future paths for intersections. How many of these intersections occur within the test area?

Your puzzle answer was 26611.
--- Part Two ---

Upon further analysis, it doesn't seem like any hailstones will naturally collide. It's up to you to fix that!

You find a rock on the ground nearby. While it seems extremely unlikely, if you throw it just right, you should be able to hit every hailstone in a single throw!

You can use the probably-magical winds to reach any integer position you like and to propel the rock at any integer velocity. Now including the Z axis in your calculations, if you throw the rock at time 0, where do you need to be so that the rock perfectly collides with every hailstone? Due to probably-magical inertia, the rock won't slow down or change direction when it collides with a hailstone.

In the example above, you can achieve this by moving to position 24, 13, 10 and throwing the rock at velocity -3, 1, 2. If you do this, you will hit every hailstone as follows:

Hailstone: 19, 13, 30 @ -2, 1, -2
Collision time: 5
Collision position: 9, 18, 20

Hailstone: 18, 19, 22 @ -1, -1, -2
Collision time: 3
Collision position: 15, 16, 16

Hailstone: 20, 25, 34 @ -2, -2, -4
Collision time: 4
Collision position: 12, 17, 18

Hailstone: 12, 31, 28 @ -1, -2, -1
Collision time: 6
Collision position: 6, 19, 22

Hailstone: 20, 19, 15 @ 1, -5, -3
Collision time: 1
Collision position: 21, 14, 12

Above, each hailstone is identified by its initial position and its velocity. Then, the time and position of that hailstone's collision with your rock are given.

After 1 nanosecond, the rock has exactly the same position as one of the hailstones, obliterating it into ice dust! Another hailstone is smashed to bits two nanoseconds after that. After a total of 6 nanoseconds, all of the hailstones have been destroyed.

So, at time 0, the rock needs to be at X position 24, Y position 13, and Z position 10. Adding these three coordinates together produces 47. (Don't add any coordinates from the rock's velocity.)

Determine the exact position and velocity the rock needs to have at time 0 so that it perfectly collides with every hailstone. What do you get if you add up the X, Y, and Z coordinates of that initial position?

Your puzzle answer was 684195328708898.

Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2324 extends Utils:

  val lines: Seq[String] = readThisObjectInputLines

  case class Hailstone(position: Position, velocity: Vector3d)
  def parseLine(line: String): Hailstone =
    line match
      case s"$px, $py, $pz @ $vx, $vy, $vz" =>
        Hailstone((px.toLong,py.toLong,pz.toLong),(vx.toLong,vy.toLong,vz.toLong))

  val hailstones = lines.map(parseLine)
//  val minX = 7L
//  val maxX = 27L
  val minX = 200_000_000_000_000L
  val maxX = 400_000_000_000_000L
  val minY = minX
  val maxY = maxX
  def intersectionPointXY(h1: Hailstone, h2: Hailstone): Option[Position] =
    val Hailstone((px1l, py1l, _), (vx1l, vy1l, _)) = h1
    val Hailstone((px2l, py2l, _), (vx2l, vy2l, _)) = h2

    val px1 = BigInt(px1l)
    val px2 = BigInt(px2l)

    val py1 = BigInt(py1l)
    val py2 = BigInt(py2l)

    val vx1 = BigInt(vx1l)
    val vx2 = BigInt(vx2l)

    val vy1 = BigInt(vy1l)
    val vy2 = BigInt(vy2l)


    val yu = py1 * vx1 * vy2 - py2 * vx2 * vy1 + (px2 - px1) * vy1 * vy2
    val xu = px1 * vy1 * vx2 - px2 * vy2 * vx1 + (py2 - py1) * vx1 * vx2

    val yd = vx1 * vy2 - vx2 * vy1
    val xd = vy1 * vx2 - vy2 * vx1

    val t1s = // (xu-px1*xd) / vx1 / xd
      (xu - px1 * xd).sign * vx1.sign * xd.sign

    val t2s = // (xu-px2*xd) / vx2 / xd
      (xu - px2 * xd).sign * vx2.sign * xd.sign

    val xur0 = minX*xd
    val xur1 = maxX*xd

    val yur0 = minY*yd
    val yur1 = maxY*yd

    if ((xd >=0 && xu >= xur0 && xu <= xur1) || (xd < 0 && xu <= xur0 && xu >= xur1)) &&
      ((yd >= 0 && yu >= yur0 && yu <= yur1) || (yd < 0 && yu <= yur0 && yu >= yur1)) &&
      t1s >= 0 &&
      t2s >= 0
    then
      Some(((xu/xd).toLong, (yu/yd).toLong, 0L))
    else
      None

  lazy val answer1: Int =
    println(hailstones.mkString("\n"))
    val points = for
      h1 <- hailstones
      h2 <- hailstones
      if h1 != h2
      i = intersectionPointXY(h1, h2)
      if i.nonEmpty
    yield
      i

    points.size / 2

  //Part 2

  import org.primetalk.advent3.tools.LinearAlgebra.*
  // -48
  lazy val answer2: BigInt =
    val i = 1
    val Hailstone(pi, vi) = hailstones(i)
    val mv6: SystemOfEquations =
      for
        j <- List(2,3,4)
        Hailstone(pj, vj) = hailstones(j)
        djiV = vj - vi
        djiP = pj - pi

        dijV = -djiV
        dijP = -djiP
        eq <- List[Row](
          List[BigInt](djiV.y, dijV.x, 0, dijP.y, djiP.x, 0, 0) -> (pj.x*vj.y-pi.x*vi.y -pj.y*vj.x+pi.y*vi.x),
          List[BigInt](djiV.z, 0, dijV.x, dijP.z, 0, djiP.x, 0) -> (pj.x*vj.z-pi.x*vi.z -pj.z*vj.x+pi.z*vi.x),
        )
      yield
        eq

    // px + py + pz + 0* v - answer2 = 0
    val mvl: Row = List[BigInt](1,1,1,0,0,0,-1) -> BigInt(0)
    val mv: SystemOfEquations = mvl :: mv6
    val t = gauss(mv)
    val (List(_,_,_,_,_,_, k), b) = t.head
    b/k
    
  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
