package org.primetalk.advent

import Geom3dUtils._
import CollectionUtils._

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

  lazy val answer1: Long = {
    val bots = nanoBots(lines)
    val strongest = strongestNanoBot(bots)
    println(strongest)
    val cnt = totalCount(strongest, bots)
    cnt
  }

  // Part 2

  def countBots(p: Vector3d, bots: Seq[NanoBot]): Int =
    bots.count(_.isWithinRange(p))

//  def findBestPosition1(bots: Seq[NanoBot]): Position = {
////    val strongest = strongestNanoBot(bots)
////    val (mean, stdDev) = meanAndStdDist(bots.map(_.position))
//    val p = boundingParallelepiped(bots.map(_.position))
//
//    def findGoodPoint(parallelepiped: Parallelepiped): (Position, Int) = {
//      val goodPoints: Seq[(Position, Int)] = (0 until 10000).map { n =>
//        val points = parallelepiped.randomPoints(1000)(new Random(n.toLong))
//        points.map(p => (p, countBots(p, bots))).maxBy(_._2)
//      }.sortBy(- _._2)
//      println(goodPoints.take(5))
//      goodPoints.head
//    }
//    val p0 = findGoodPoint(p)
////    def go(position: Position, cnt: Int, s: Vector3d): Position = {
////      val size2 = s / 2
////      val parallelepiped = Parallelepiped(position - size2, position + size2)
////      val (pp, cnt2) = findGoodPoint(parallelepiped)
////      if(cnt2 > cnt)
////        go(pp, cnt2, s)
////      else pp
////    }
////    go(p0._1, p0._2, p.size)
////    val p = Parallelepiped(mean - (stdDev * 1), mean + (stdDev * 1))
////    val points: Seq[Position] = (0 until 10000).flatMap(n => p.randomPoints(100)(new Random(n.toLong)))
////    val counts = points.map(p => (p, countBots(p, bots))).sortBy(- _._2)
////    println(counts.take(5))
////    counts.head._1
//
//    p0._1
//  }
//
//  def findBestPosition2(bots: Seq[Day23.NanoBot]): Position = {
//    val spheres: Array[ManhattanParallelepiped] = bots.map(bot => manhattanSphere(bot.position, bot.radius)).toArray.sortBy(- _.manhattanSize)
////    val points = spheres.flatMap(p => Seq(p.topLeft, p.bottomRight))
////
////    type SphereId = Int
////    val allIds = spheres.indices.toSet
////    type State = (ManhattanParallelepiped, Set[SphereId])
////    val initialState = (boundingParallelepiped(points), Set())
//    def go(states: Seq[ManhattanParallelepiped]): Set[ManhattanParallelepiped] = {
//      println(states.size)
//      val pairwiseIntersections =
//        for{
//          s1 <- states
//          _ = {System.gc()}
//          s2 <- states
//          if s1 != s2
//          i <- s1.intersect(s2)
//      } yield i
//      if(pairwiseIntersections.isEmpty)
//        states.toSet
//      else
//        go(pairwiseIntersections.sortBy(- _.manhattanSize).take(200))
//    }
//    val res = go(spheres.take(11))
//    println(res.mkString("\n"))
////    val graph: GraphAsFunction[State] = {
////      case (p, currentIds) =>
////        (allIds -- currentIds).toSeq.flatMap {
////          id =>
////            val otherP = spheres(id)
////            p.intersect(otherP).map {
////              newP =>
////                (newP, currentIds.+(id))
////            }
////        }
////    }
////    findMax(graph)(initialState)
//    origin
////      ???
//  }

  def findBestPosition3(bots: Seq[Day23.NanoBot]): Seq[Position] = {
    val spheres: Array[ManhattanParallelepiped] = bots.map(bot => manhattanSphere(bot.position, bot.radius)).toArray
    def countParall(p: ManhattanParallelepiped): Int =
      spheres.count(sp => p.intersect(sp).nonEmpty)
    def go(toConsider: Vector[(ManhattanParallelepiped, Int)], countLimit: Int, found: List[ManhattanParallelepiped] = Nil): List[ManhattanParallelepiped] =
      if(toConsider.isEmpty)
        found.filter(p => countParall(p) >= countLimit)
      else {
        val (pp, cnt) = toConsider.head
        val tail = toConsider.tail
        if(cnt < countLimit) {
          go(tail, countLimit, found)
        } else if(pp.manhattanSize <= 3)
          go(tail, countLimit, pp :: found)
        else{
          val cnt = countParall(Parallelepiped(pp.topLeft, pp.topLeft))
          val nextCountLimit = cnt.max(countLimit) // there exist at least a point that has this number of bots around
          if(nextCountLimit > countLimit) println(nextCountLimit)
          val parallelepipeds = pp.divideIntoSmallerPieces(2)
          if(parallelepipeds.size == 1) {
            require(parallelepipeds.head == pp)
            go(tail, nextCountLimit, pp :: found)
          } else {
            val subdivisions = parallelepipeds.map(ppp => (ppp, countParall(ppp)))
            go(insertAllIntoSortedVector(tail, subdivisions)(Ordering.by(-_._2)), nextCountLimit, found)
          }
        }
      }
    val points = spheres.flatMap(p => Seq(p.topLeft, p.bottomRight))
    val initialArea = boundingParallelepiped(points)
    val initialCount = countParall(initialArea)
    require(bots.size == initialCount) // Our initial area is just very big
    val initialCountLimit = 1 // 976 // to speedup we may set 976, because it yields the same results.
    val found1 =
      GraphUtils.searchForMaximum(initialArea)(pp => countParall(Parallelepiped(pp.topLeft, pp.topLeft)), countParall)(_.divideIntoSmallerPieces(2))
    println(found1)
//    val found = go(Vector((initialArea, initialCount)), initialCountLimit)
//    println(found)
    found1.map(_._1.topLeft).map(fromManhattan).distinct


  }
  // 678, 824, 871,
  // 883 (0 until 10000).flatMap(n => p.randomPoints(100)(new Random(n.toLong)))
  // 53546326 - too low
  // 138697279 - too low
  // 138697281
  lazy val answer2: Long = {
    val bots = nanoBots(lines)
    val bestPositions = findBestPosition3(bots)
    val bestPos = bestPositions.maxBy(countBots(_, bots))
//    val cnt = countBots(bestPos, bots)
//    println(cnt) // 971 - 976
    manhattanDistance(origin, bestPos).toLong
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
