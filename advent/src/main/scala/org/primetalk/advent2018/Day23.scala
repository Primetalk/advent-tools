package org.primetalk.advent2018

import org.primetalk.advent.tools.Geom3dUtils._
import org.primetalk.advent.tools.{GraphUtils, Utils}

/**
  * --- Day 23: Experimental Emergency Teleportation ---
  *
  * Using your torch to search the darkness of the rocky cavern, you finally locate the man's friend: a small reindeer.
  *
  * You're not sure how it got so far in this cave. It looks sick - too sick to walk - and too heavy for you to carry all the way back. Sleighs won't be invented for another 1500 years, of course.
  *
  * The only option is experimental emergency teleportation.
  *
  * You hit the "experimental emergency teleportation" button on the device and push I accept the risk on no fewer than 18 different warning messages. Immediately, the device deploys hundreds of tiny nanobots which fly around the cavern, apparently assembling themselves into a very specific formation. The device lists the X,Y,Z position (pos) for each nanobot as well as its signal radius (r) on its tiny screen (your puzzle input).
  *
  * Each nanobot can transmit signals to any integer coordinate which is a distance away from it less than or equal to its signal radius (as measured by Manhattan distance). Coordinates a distance away of less than or equal to a nanobot's signal radius are said to be in range of that nanobot.
  *
  * Before you start the teleportation process, you should determine which nanobot is the strongest (that is, which has the largest signal radius) and then, for that nanobot, the total number of nanobots that are in range of it, including itself.
  *
  * For example, given the following nanobots:
  *
  * pos=<0,0,0>, r=4
  * pos=<1,0,0>, r=1
  * pos=<4,0,0>, r=3
  * pos=<0,2,0>, r=1
  * pos=<0,5,0>, r=3
  * pos=<0,0,3>, r=1
  * pos=<1,1,1>, r=1
  * pos=<1,1,2>, r=1
  * pos=<1,3,1>, r=1
  *
  * The strongest nanobot is the first one (position 0,0,0) because its signal radius, 4 is the largest. Using that nanobot's location and signal radius, the following nanobots are in or out of range:
  *
  * The nanobot at 0,0,0 is distance 0 away, and so it is in range.
  * The nanobot at 1,0,0 is distance 1 away, and so it is in range.
  * The nanobot at 4,0,0 is distance 4 away, and so it is in range.
  * The nanobot at 0,2,0 is distance 2 away, and so it is in range.
  * The nanobot at 0,5,0 is distance 5 away, and so it is not in range.
  * The nanobot at 0,0,3 is distance 3 away, and so it is in range.
  * The nanobot at 1,1,1 is distance 3 away, and so it is in range.
  * The nanobot at 1,1,2 is distance 4 away, and so it is in range.
  * The nanobot at 1,3,1 is distance 5 away, and so it is not in range.
  *
  * In this example, in total, 7 nanobots are in range of the nanobot with the largest signal radius.
  *
  * Find the nanobot with the largest signal radius. How many nanobots are in range of its signals?
  *
  * Your puzzle answer was 935.
  * --- Part Two ---
  *
  * Now, you just need to figure out where to position yourself so that you're actually teleported when the nanobots activate.
  *
  * To increase the probability of success, you need to find the coordinate which puts you in range of the largest number of nanobots. If there are multiple, choose one closest to your position (0,0,0, measured by manhattan distance).
  *
  * For example, given the following nanobot formation:
  *
  * pos=<10,12,12>, r=2
  * pos=<12,14,12>, r=2
  * pos=<16,12,12>, r=4
  * pos=<14,14,14>, r=6
  * pos=<50,50,50>, r=200
  * pos=<10,10,10>, r=5
  *
  * Many coordinates are in range of some of the nanobots in this formation. However, only the coordinate 12,12,12 is in range of the most nanobots: it is in range of the first five, but is not in range of the nanobot at 10,10,10. (All other coordinates are in range of fewer than five nanobots.) This coordinate's distance from 0,0,0 is 36.
  *
  * Find the coordinates that are in range of the largest number of nanobots. What is the shortest manhattan distance between any of those points and 0,0,0?
  *
  * Your puzzle answer was 138697281.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day23 extends Utils {

  lazy val inputTextFromResource : Iterator[String] =
    readResource("day23.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq

  case class NanoBot(position: Vector3d, radius: Int) {
    def isWithinRange(otherPosition: Vector3d): Boolean =
      manhattanDistance(position, otherPosition) <= radius
  }

  def parseNanoBot(line: String): NanoBot = {
    val Seq(x,y,z,r) = parseAllIntsInString(line)
    NanoBot((x,y,z),r)
  }

  def nanoBots(lines: Seq[String]): Seq[NanoBot] = lines.map(parseNanoBot)

  def strongestNanoBot(bots: Seq[NanoBot]): NanoBot = bots.maxBy(_.radius)

  def totalCount(strongest: NanoBot, bots: Seq[NanoBot]): Int =
    bots.count(b => strongest.isWithinRange(b.position))

  lazy val answer1: Int = {
    val bots = nanoBots(lines)
    val strongest = strongestNanoBot(bots)
    val cnt = totalCount(strongest, bots)
    cnt
  }

  // Part 2

  def countBots(p: Vector3d, bots: Seq[NanoBot]): Int =
    bots.count(_.isWithinRange(p))

  def findBestPosition3(bots: Seq[Day23.NanoBot]): Seq[Position] = {
    val spheres: Array[ManhattanParallelepiped] =
      bots.map(bot => manhattanSphere(bot.position, bot.radius)).toArray
    def countBotsThatIntersectParallelepiped(p: ManhattanParallelepiped): Int =
      spheres.count(sp => p.intersect(sp).nonEmpty)
    val points = spheres.flatMap(p => Seq(p.topLeft, p.bottomRight)).toIndexedSeq
    val initialArea = boundingParallelepiped(points)
    // to speedup we may start with 976, because it yields the same results.
    val found1 =
      GraphUtils.searchForMaximum(initialArea)(pp => countBotsThatIntersectParallelepiped(Parallelepiped(pp.topLeft, (0,0,0))), countBotsThatIntersectParallelepiped)(_.divideIntoSmallerPieces(2))
    println(found1)
    found1.map(_._1.topLeft).map(fromManhattan).distinct
  }
  // 678, 824, 871,
  // 883 (0 until 10000).flatMap(n => p.randomPoints(100)(new Random(n.toLong)))
  // 53546326 - too low
  // 138697279 - too low
  // 138697281
  lazy val answer2: Int = {
    val bots = nanoBots(lines)
    val bestPositions = findBestPosition3(bots)
    val counts = bestPositions.map(countBots(_, bots))
    val bestPos = bestPositions.maxBy(countBots(_, bots))
    manhattanDistance(origin, bestPos)
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
