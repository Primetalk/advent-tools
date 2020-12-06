package org.primetalk.advent2020

import org.primetalk.advent.tools.Utils
import org.primetalk.advent2020.Day2002.readResourceLines

import scala.util.matching.Regex

/**
  * https://adventofcode.com/2020/day/3
  *
--- Day 3: Toboggan Trajectory ---

With the toboggan login problems resolved, you set off toward the airport. While travel by toboggan might be easy, it's certainly not safe: there's very minimal steering and the area is covered in trees. You'll need to see which angles will take you near the fewest trees.

Due to the local geology, trees in this area only grow on exact integer coordinates in a grid. You make a map (your puzzle input) of the open squares (.) and trees (#) you can see. For example:

..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#

These aren't the only trees, though; due to something you read about once involving arboreal genetics and biome stability, the same pattern repeats to the right many times:

..##.........##.........##.........##.........##.........##.......  --->
#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........#.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...##....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->

You start on the open square (.) in the top-left corner and need to reach the bottom (below the bottom-most row on your map).

The toboggan can only follow a few specific slopes (you opted for a cheaper model that prefers rational numbers); start by counting all the trees you would encounter for the slope right 3, down 1:

From your starting position at the top-left, check the position that is right 3 and down 1. Then, check the position that is right 3 and down 1 from there, and so on until you go past the bottom of the map.

The locations you'd check in the above example are marked here with O where there was an open square and X where there was a tree:

..##.........##.........##.........##.........##.........##.......  --->
#..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
.#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........X.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...#X....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->

In this example, traversing the map using this slope would cause you to encounter 7 trees.

Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?

  */
object Day2003 extends Utils {

  lazy val inputTextFromResource: IndexedSeq[String] =
    readResourceLines("day3.txt")

  def countTreesWithSlope(slope_x: Int, slope_y: Int): Int = {

    println(s"Slope($slope_x, $slope_y)")
    val linesToCheck =
//      if(slope_y == 1)
      inputTextFromResource.drop(1)
//    else {
//      inputTextFromResource.drop(1).zipWithIndex
//        .collect{ case (line, y) if y % slope_y == 0 => line}
//    }
    linesToCheck.zipWithIndex.count{
      case (line, ym1) =>
        val y = ym1 + 2
        val x = slope_x * (y - 1) / slope_y + 1
        val pos = (x - 1) % line.length

        ((y - 1) % slope_y == 0) && {
          val c = line.charAt(pos)
          val rep = if(c == '#') 'X' else 'O'
          val line2 = line.substring(0, pos) + rep + line.substring(pos + 1)
          println(line2)
          c == '#'
        }
    }
  }
  // 7, 259
  lazy val answer1: Int = countTreesWithSlope(3, 1)

  // Part 2
  // 2288482560, 1716361920
  lazy val answer2: Long = {
    val slopes = Seq(
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2),
    )
    val treeCounts = slopes.map{ case (x,y) => countTreesWithSlope(x,y).toLong}
    println(treeCounts)
    treeCounts.product
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }
}