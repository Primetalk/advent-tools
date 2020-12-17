package org.primetalk.advent2020

import org.primetalk.advent.tools.{Display, Display3D, Display4D}

import scala.annotation.tailrec

/**
  * https://adventofcode.com/2020/day/17
  * --- Day 17: Conway Cubes ---
  *
  * As your flight slowly drifts through the sky, the Elves at the Mythical Information Bureau at the North Pole contact you. They'd like some help debugging a malfunctioning experimental energy source aboard one of their super-secret imaging satellites.
  *
  * The experimental energy source is based on cutting-edge technology: a set of Conway Cubes contained in a pocket dimension! When you hear it's having problems, you can't help but agree to take a look.
  *
  * The pocket dimension contains an infinite 3-dimensional grid. At every integer 3-dimensional coordinate (x,y,z), there exists a single cube which is either active or inactive.
  *
  * In the initial state of the pocket dimension, almost all cubes start inactive. The only exception to this is a small flat region of cubes (your puzzle input); the cubes in this region start in the specified active (#) or inactive (.) state.
  *
  * The energy source then proceeds to boot up by executing six cycles.
  *
  * Each cube only ever considers its neighbors: any of the 26 other cubes where any of their coordinates differ by at most 1. For example, given the cube at x=1,y=2,z=3, its neighbors include the cube at x=2,y=2,z=2, the cube at x=0,y=2,z=3, and so on.
  *
  * During a cycle, all cubes simultaneously change their state according to the following rules:
  *
  *     If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
  *     If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.
  *
  * The engineers responsible for this experimental energy source would like you to simulate the pocket dimension and determine what the configuration of cubes should be at the end of the six-cycle boot process.
  *
  * For example, consider the following initial state:
  *
  * .#.
  * ..#
  * ###
  *
  * Even though the pocket dimension is 3-dimensional, this initial state represents a small 2-dimensional slice of it. (In particular, this initial state defines a 3x3x1 region of the 3-dimensional space.)
  *
  * Simulating a few cycles from this initial state produces the following configurations, where the result of each cycle is shown layer-by-layer at each given z coordinate (and the frame of view follows the active cells in each cycle):
  *
  * Before any cycles:
  *
  * z=0
  * .#.
  * ..#
  * ###
  *
  *
  * After 1 cycle:
  *
  * z=-1
  * #..
  * ..#
  * .#.
  *
  * z=0
  * #.#
  * .##
  * .#.
  *
  * z=1
  * #..
  * ..#
  * .#.
  *
  *
  * After 2 cycles:
  *
  * z=-2
  * .....
  * .....
  * ..#..
  * .....
  * .....
  *
  * z=-1
  * ..#..
  * .#..#
  * ....#
  * .#...
  * .....
  *
  * z=0
  * ##...
  * ##...
  * #....
  * ....#
  * .###.
  *
  * z=1
  * ..#..
  * .#..#
  * ....#
  * .#...
  * .....
  *
  * z=2
  * .....
  * .....
  * ..#..
  * .....
  * .....
  *
  *
  * After 3 cycles:
  *
  * z=-2
  * .......
  * .......
  * ..##...
  * ..###..
  * .......
  * .......
  * .......
  *
  * z=-1
  * ..#....
  * ...#...
  * #......
  * .....##
  * .#...#.
  * ..#.#..
  * ...#...
  *
  * z=0
  * ...#...
  * .......
  * #......
  * .......
  * .....##
  * .##.#..
  * ...#...
  *
  * z=1
  * ..#....
  * ...#...
  * #......
  * .....##
  * .#...#.
  * ..#.#..
  * ...#...
  *
  * z=2
  * .......
  * .......
  * ..##...
  * ..###..
  * .......
  * .......
  * .......
  *
  * After the full six-cycle boot process completes, 112 cubes are left in the active state.
  *
  * Starting with your given initial configuration, simulate six cycles. How many cubes are left in the active state after the sixth cycle?
  *
  * Your puzzle answer was 310.
  * --- Part Two ---
  *
  * For some reason, your simulated results don't match what the experimental energy source engineers expected. Apparently, the pocket dimension actually has four spatial dimensions, not three.
  *
  * The pocket dimension contains an infinite 4-dimensional grid. At every integer 4-dimensional coordinate (x,y,z,w), there exists a single cube (really, a hypercube) which is still either active or inactive.
  *
  * Each cube only ever considers its neighbors: any of the 80 other cubes where any of their coordinates differ by at most 1. For example, given the cube at x=1,y=2,z=3,w=4, its neighbors include the cube at x=2,y=2,z=3,w=3, the cube at x=0,y=2,z=3,w=4, and so on.
  *
  * The initial state of the pocket dimension still consists of a small flat region of cubes. Furthermore, the same rules for cycle updating still apply: during each cycle, consider the number of active neighbors of each cube.
  *
  * For example, consider the same initial state as in the example above. Even though the pocket dimension is 4-dimensional, this initial state represents a small 2-dimensional slice of it. (In particular, this initial state defines a 3x3x1x1 region of the 4-dimensional space.)
  *
  * Simulating a few cycles from this initial state produces the following configurations, where the result of each cycle is shown layer-by-layer at each given z and w coordinate:
  *
  * Before any cycles:
  *
  * z=0, w=0
  * .#.
  * ..#
  * ###
  *
  *
  * After 1 cycle:
  *
  * z=-1, w=-1
  * #..
  * ..#
  * .#.
  *
  * z=0, w=-1
  * #..
  * ..#
  * .#.
  *
  * z=1, w=-1
  * #..
  * ..#
  * .#.
  *
  * z=-1, w=0
  * #..
  * ..#
  * .#.
  *
  * z=0, w=0
  * #.#
  * .##
  * .#.
  *
  * z=1, w=0
  * #..
  * ..#
  * .#.
  *
  * z=-1, w=1
  * #..
  * ..#
  * .#.
  *
  * z=0, w=1
  * #..
  * ..#
  * .#.
  *
  * z=1, w=1
  * #..
  * ..#
  * .#.
  *
  *
  * After 2 cycles:
  *
  * z=-2, w=-2
  * .....
  * .....
  * ..#..
  * .....
  * .....
  *
  * z=-1, w=-2
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=0, w=-2
  * ###..
  * ##.##
  * #...#
  * .#..#
  * .###.
  *
  * z=1, w=-2
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=2, w=-2
  * .....
  * .....
  * ..#..
  * .....
  * .....
  *
  * z=-2, w=-1
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=-1, w=-1
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=0, w=-1
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=1, w=-1
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=2, w=-1
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=-2, w=0
  * ###..
  * ##.##
  * #...#
  * .#..#
  * .###.
  *
  * z=-1, w=0
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=0, w=0
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=1, w=0
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=2, w=0
  * ###..
  * ##.##
  * #...#
  * .#..#
  * .###.
  *
  * z=-2, w=1
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=-1, w=1
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=0, w=1
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=1, w=1
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=2, w=1
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=-2, w=2
  * .....
  * .....
  * ..#..
  * .....
  * .....
  *
  * z=-1, w=2
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=0, w=2
  * ###..
  * ##.##
  * #...#
  * .#..#
  * .###.
  *
  * z=1, w=2
  * .....
  * .....
  * .....
  * .....
  * .....
  *
  * z=2, w=2
  * .....
  * .....
  * ..#..
  * .....
  * .....
  *
  * After the full six-cycle boot process completes, 848 cubes are left in the active state.
  *
  * Starting with your given initial configuration, simulate six cycles in a 4-dimensional space. How many cubes are left in the active state after the sixth cycle?
  *
  * Your puzzle answer was 2056.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2017 {

  def task1(input: String, expectedSteps: Int = 6): Int = {
    val lines = input.split('\n')
      .toIndexedSeq

    val initialSize: Int = lines.size

    val layer0: Display[Char] = Display.readCharDisplay(
      lines, '.')
      .offsetByInPlace((-initialSize/2, -initialSize/2))

    val expectedSize: Int = initialSize + expectedSteps * 2
    val initialDisplay3d: Display3D[Char] = {
      val d = new Display3D[Char](
        (-expectedSteps - initialSize/2, -expectedSteps - initialSize/2, -expectedSteps - initialSize/2),
        (expectedSize, expectedSize, expectedSize)
      )
      val enlarged = layer0.enlargeBy(expectedSteps)
      d.writeLayer(0, enlarged)
      d
    }

    def rule(c: Char, chars: Seq[Char]): Char = {
      val cnt = chars.count(_ == '#')
      if (c == '#') {
        if (cnt == 2 || cnt == 3)
          c
        else
          '.'
      } else {
        if (cnt == 3)
          '#'
        else
          c
      }
    }

    @tailrec
    def runRules(display3d: Display3D[Char], count: Int): Display3D[Char] = {
      if (count == 0)
        display3d
      else {
        val next = display3d.produceByLocalRules(rule)
        runRules(next, count - 1)
      }
    }
    val f = runRules(initialDisplay3d, 6)
    f.values.count(_ == '#')
  }

  val originalInput: String =
    """##.#####
      |#.##..#.
      |.##...##
      |###.#...
      |.#######
      |##....##
      |###.###.
      |.#.#.#..
      |""".stripMargin
  // 167, 310
  lazy val answer1: Int = task1(originalInput)

  //Part 2

  def task2(input: String, expectedSteps: Int = 6): Int = {
    val lines = input.split('\n')
      .toIndexedSeq

    val initialSize: Int = lines.size

    val layer0: Display[Char] = Display.readCharDisplay(
      lines, '.')
      .offsetByInPlace((-initialSize/2, -initialSize/2))

    val expectedSize: Int = initialSize + expectedSteps * 2

    val initialDisplay3d: Display3D[Char] = {
      val d = new Display3D[Char](
        (-expectedSteps - initialSize/2, -expectedSteps - initialSize/2, -expectedSteps - initialSize/2),
        (expectedSize, expectedSize, expectedSize)
      )
      val enlarged = layer0.enlargeBy(expectedSteps)
      d.writeLayer(0, enlarged)
      d
    }

    val initialDisplay4d: Display4D[Char] = {
      val d = new Display4D[Char](
        (-expectedSteps - initialSize/2, -expectedSteps - initialSize/2, -expectedSteps - initialSize/2, -expectedSteps - initialSize/2),
        (expectedSize, expectedSize, expectedSize, expectedSize)
      )
      d.writeCubeInPlace(0, initialDisplay3d)
      d
    }

    def rule(c: Char, chars: Seq[Char]): Char = {
      val cnt = chars.count(_ == '#')
      if (c == '#') {
        if (cnt == 2 || cnt == 3)
          c
        else
          '.'
      } else {
        if (cnt == 3)
          '#'
        else
          c
      }
    }

    @tailrec
    def runRules(display4d: Display4D[Char], count: Int): Display4D[Char] = {
      if (count == 0)
        display4d
      else {
        val next = display4d.produceByLocalRules(rule)
        runRules(next, count - 1)
      }
    }
    val f = runRules(initialDisplay4d, 6)
    f.values.count(_ == '#')
  }
  lazy val answer2: Int = task2(originalInput)

  def main(args: Array[String]): Unit = {
//    println(task1(
//      """.#.
//        |..#
//        |###
//        |""".stripMargin))
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
