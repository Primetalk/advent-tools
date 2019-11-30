package org.primetalk.advent2018

import org.primetalk.advent.tools.Geom2dUtils._
import org.primetalk.advent.tools.Utils

/**
  * --- Day 6: Chronal Coordinates ---
  *
  * The device on your wrist beeps several times, and once again you feel like you're falling.
  *
  * "Situation critical," the device announces. "Destination indeterminate. Chronal interference detected. Please specify new target coordinates."
  *
  * The device then produces a list of coordinates (your puzzle input). Are they places it thinks are safe or dangerous? It recommends you check manual page 729. The Elves did not give you a manual.
  *
  * If they're dangerous, maybe you can minimize the danger by finding the coordinate that gives the largest distance from the other points.
  *
  * Using only the Manhattan distance, determine the area around each coordinate by counting the number of integer X,Y locations that are closest to that coordinate (and aren't tied in distance to any other coordinate).
  *
  * Your goal is to find the size of the largest area that isn't infinite. For example, consider the following list of coordinates:
  *
  * 1, 1
  * 1, 6
  * 8, 3
  * 3, 4
  * 5, 5
  * 8, 9
  *
  * If we name these coordinates A through F, we can draw them on a grid, putting 0,0 at the top left:
  *
  * ..........
  * .A........
  * ..........
  * ........C.
  * ...D......
  * .....E....
  * .B........
  * ..........
  * ..........
  * ........F.
  *
  * This view is partial - the actual grid extends infinitely in all directions. Using the Manhattan distance, each location's closest coordinate can be determined, shown here in lowercase:
  *
  * aaaaa.cccc
  * aAaaa.cccc
  * aaaddecccc
  * aadddeccCc
  * ..dDdeeccc
  * bb.deEeecc
  * bBb.eeee..
  * bbb.eeefff
  * bbb.eeffff
  * bbb.ffffFf
  *
  * Locations shown as . are equally far from two or more coordinates, and so they don't count as being closest to any.
  *
  * In this example, the areas of coordinates A, B, C, and F are infinite - while not shown here, their areas extend forever outside the visible grid. However, the areas of coordinates D and E are finite: D is closest to 9 locations, and E is closest to 17 (both including the coordinate's location itself). Therefore, in this example, the size of the largest area is 17.
  *
  * What is the size of the largest area that isn't infinite?
  *
  * Your puzzle answer was 3010.
  * --- Part Two ---
  *
  * On the other hand, if the coordinates are safe, maybe the best you can do is try to find a region near as many coordinates as possible.
  *
  * For example, suppose you want the sum of the Manhattan distance to all of the coordinates to be less than 32. For each location, add up the distances to all of the given coordinates; if the total of those distances is less than 32, that location is within the desired region. Using the same coordinates as above, the resulting region looks like this:
  *
  * ..........
  * .A........
  * ..........
  * ...###..C.
  * ..#D###...
  * ..###E#...
  * .B.###....
  * ..........
  * ..........
  * ........F.
  *
  * In particular, consider the highlighted location 4,3 located at the top middle of the region. Its calculation is as follows, where abs() is the absolute value function:
  *
  * Distance to coordinate A: abs(4-1) + abs(3-1) =  5
  * Distance to coordinate B: abs(4-1) + abs(3-6) =  6
  * Distance to coordinate C: abs(4-8) + abs(3-3) =  4
  * Distance to coordinate D: abs(4-3) + abs(3-4) =  2
  * Distance to coordinate E: abs(4-5) + abs(3-5) =  3
  * Distance to coordinate F: abs(4-8) + abs(3-9) = 10
  * Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30
  *
  * Because the total distance to all coordinates (30) is less than 32, the location is within the region.
  *
  * This region, which also includes coordinates D and E, has a total size of 16.
  *
  * Your actual region will need to be much larger than this example, though, instead including all locations with a total distance of less than 10000.
  *
  * What is the size of the region containing all locations which have a total distance to all given coordinates of less than 10000?
  *
  * Your puzzle answer was 48034.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day6 extends Utils {
  lazy val inputTextFromResource : Iterator[String] =
    readResource("day6.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq //.sorted

  def parseCoordinates(l: String): Position = {
    val list = parseAllIntsInString(l).toList
    val List(x, y) = list
    (x,y)
  }

  lazy val coordinates: Seq[(Int, Int)] = lines.filter(_.nonEmpty).map(parseCoordinates)

  def putOnDisplay(coordinates: Seq[(Int, Int)], display: Display[Int]): Unit = {
    coordinates.zipWithIndex.foreach{ case ((x, y), ind) => display((x,y)) = ind }
  }

  def markAreas(display: Display[Int], coordinates: Seq[(Int, Int)]): Unit = {
    display.points.foreach{ p =>
        val distances = coordinates.zipWithIndex.map {
          case (c, ind) =>
            (ind, manhattanDistance(p, c))
        }
        val minDist = distances.minBy(_._2)._2
        val minimals = distances.filter(_._2 == minDist)

        val closestInd = if(minimals.size == 1) minimals.head._1 else -1
        display(p) = closestInd
    }
  }

  def areaSizes(display: Display[Int]): Seq[(Int, Int)] = {
    display.values.groupBy(identity).toSeq
//      .filter(_._1 != -1)
      .sortBy(_._1)
      .map{ case (ind, seq) => (ind, seq.size) }
  }

  def largest(sizes: Seq[(Int, Int)]): Int = {
    sizes.maxBy(_._2)._2
  }

  def answer1: Int = 0
//  {
//    val d = Display[Int]((0, 0), (400, 400))
//    markAreas(d, coordinates)
//    val sizes = areaSizes(d)
//    val edges: Set[Int] = d.valuesOnEdges
//    val withoutEdges = sizes.filterNot{ case (ind, _) => edges.contains(ind)}
//    val (indM, sizeM) = withoutEdges.maxBy(_._2)
//    sizeM // largest(withoutEdges)
//  }

  // Part 2

  def calcRegion(display: Display[Int], coordinates: Seq[(Int, Int)], boundary: Int = 10000): Int = {
    var cnt = 0
    display.points.foreach{ p =>
      val distances = coordinates.zipWithIndex.map {
        case (c, ind) =>
          manhattanDistance(p, c)
      }
      val s = distances.sum
      cnt += (if(s < boundary) 1 else 0)
    }
    cnt
  }



  def answer2: Int = {
    val d = Display[Int]((0, 0), (400, 400))()
    calcRegion(d, coordinates)
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }
}
