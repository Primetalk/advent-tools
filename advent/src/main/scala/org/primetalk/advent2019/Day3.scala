package org.primetalk.advent2019

import org.primetalk.advent.tools.{Display, Geom2dUtils, Utils}
import org.primetalk.advent.tools.Geom2dUtils._
/**
  * https://adventofcode.com/2019/day/3
  *
  * --- Day 3: Crossed Wires ---
  *
  * The gravity assist was successful, and you're well on your way to the Venus refuelling station. During the rush back on Earth, the fuel management system wasn't completely installed, so that's next on the priority list.
  *
  * Opening the front panel reveals a jumble of wires. Specifically, two wires are connected to a central port and extend outward on a grid. You trace the path each wire takes as it leaves the central port, one wire per line of text (your puzzle input).
  *
  * The wires twist and turn, but the two wires occasionally cross paths. To fix the circuit, you need to find the intersection point closest to the central port. Because the wires are on a grid, use the Manhattan distance for this measurement. While the wires do technically cross right at the central port where they both start, this point does not count, nor does a wire count as crossing with itself.
  *
  * For example, if the first wire's path is R8,U5,L5,D3, then starting from the central port (o), it goes right 8, up 5, left 5, and finally down 3:
  *
  * ...........
  * ...........
  * ...........
  * ....+----+.
  * ....|....|.
  * ....|....|.
  * ....|....|.
  * .........|.
  * .o-------+.
  * ...........
  *
  * Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4, and left 4:
  *
  * ...........
  * .+-----+...
  * .|.....|...
  * .|..+--X-+.
  * .|..|..|.|.
  * .|.-X--+.|.
  * .|..|....|.
  * .|.......|.
  * .o-------+.
  * ...........
  *
  * These wires cross at two locations (marked X), but the lower-left one is closer to the central port: its distance is 3 + 3 = 6.
  *
  * Here are a few more examples:
  *
  * R75,D30,R83,U83,L12,D49,R71,U7,L72
  * U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
  * R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
  * U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
  *
  * What is the Manhattan distance from the central port to the closest intersection?
  *
  * Your puzzle answer was 2050.
  * --- Part Two ---
  *
  * It turns out that this circuit is very timing-sensitive; you actually need to minimize the signal delay.
  *
  * To do this, calculate the number of steps each wire takes to reach each intersection; choose the intersection where the sum of both wires' steps is lowest. If a wire visits a position on the grid multiple times, use the steps value from the first time it visits that position when calculating the total value of a specific intersection.
  *
  * The number of steps a wire takes is the total number of grid squares the wire has entered to get to that location, including the intersection being considered. Again consider the example from above:
  *
  * ...........
  * .+-----+...
  * .|.....|...
  * .|..+--X-+.
  * .|..|..|.|.
  * .|.-X--+.|.
  * .|..|....|.
  * .|.......|.
  * .o-------+.
  * ...........
  *
  * In the above example, the intersection closest to the central port is reached after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 = 20 steps by the second wire for a total of 20+20 = 40 steps.
  *
  * However, the top-right intersection is better: the first wire takes only 8+5+2 = 15 and the second wire takes only 7+6+2 = 15, a total of 15+15 = 30 steps.
  *
  * Here are the best steps for the extra examples from above:
  *
  * R75,D30,R83,U83,L12,D49,R71,U7,L72
  * U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
  * R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
  * U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps
  *
  * What is the fewest combined steps the wires must take to reach an intersection?
  *
  * Your puzzle answer was 21666.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day3 extends Utils {

  lazy val inputTextFromResource : Iterator[String] =
    readResource("day3.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq

  lazy val wire1: String = lines.head

  lazy val wire2: String = lines(1)

  def parseDirVector(s: String): DirVector =
    DirVector(charToDirection(s.charAt(0)), s.substring(1).toInt)

  def parseWire(w: String): List[DirVector] =
    w.split(',').map(parseDirVector).toList

  val drawingChars: List[Char] = "|-+x".toList

  def drawingCharForDirection(d: Direction): Char = d match {
    case Up | Down => '|'
    case Left | Right => '-'
    case _ => 'x'
  }

  val maxSize = 2000

  def renderWire(w: List[DirVector]): Display[Char] = {
    val d = Display[Char](Rectangle((-maxSize,-maxSize), (2*maxSize + 1,2*maxSize + 1)))
    def loop(pos: Position, rest: List[DirVector]): Display[Char] = rest match {
      case Nil => d
      case h :: t =>
        val ch = drawingCharForDirection(h.direction)
        (1 until h.length).foreach{ (i: Int) =>
          val p = pos + (new VecOps(h.direction:Vector2d) * i)
          d.safeUpdate(p, ch)
        }
        val nextPos = pos + h.toVector2d
        d.safeUpdate(nextPos, '+')
        loop(nextPos, t)
    }
    loop(Geom2dUtils.origin, w)
  }

  def findCrosses(d1: Display[Char], d2: Display[Char]): Seq[Position] = {
    d1.points.filter{
      p =>
        drawingChars.contains(d1(p)) && drawingChars.contains(d2(p))
    }
  }

  def findLowestDistance(positions: Seq[Position]): Int =
    positions.map(_.manhattanSize).min

  def minCrossDistance(wire1: String, wire2: String): Int = {
// using display. It is not suitable for the second part.
//    val d1 = renderWire(parseWire(wire1))
//    //    println(d1.showDisplay()())
//    val d2 = renderWire(parseWire(wire2))
//    //    println(d2.showDisplay()())
//    findLowestDistance(findCrosses(d1, d2))
    val points1 = wireToPoints(parseWire(wire1))
    val points2 = wireToPoints(parseWire(wire2))
    val set1 = points1.toSet
    val set2 = points2.toSet
    val intersections = set1.intersect(set2)
    intersections.map(_.manhattanSize).min
  }

  lazy val answer1: Long = {
    minCrossDistance(wire1, wire2)
  }

  // Part 2
  // converts wires to positions.
  def wireToPoints(w: List[DirVector]): List[Position] = {
    def loop(pos: Position, rest: List[DirVector], positions: List[Position]): List[Position] = rest match {
      case Nil => positions.reverse
      case h :: t =>
        val nextPositions = h.drawPrependFromStart(pos, positions)
        loop(nextPositions.head, t, nextPositions)
    }
    loop(Geom2dUtils.origin, w, Nil)
  }

  def minSignalDelay(wire1: String, wire2: String): Int = {
    val points1 = wireToPoints(parseWire(wire1))
    val points2 = wireToPoints(parseWire(wire2))
    val set1 = points1.toSet
    val set2 = points2.toSet
    val intersections = set1.intersect(set2)
    val steps = intersections.map{ p =>
      val steps1 = points1.indexOf(p) + 1 // + 1 because the first element has index 1, but requires 1 step from the origin
      val steps2 = points2.indexOf(p) + 1
      steps1 + steps2
    }
    steps.min
  }

  lazy val answer2: Long = minSignalDelay(wire1, wire2)
  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
