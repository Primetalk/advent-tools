package org.primetalk.advent2019

import org.primetalk.advent.tools.Geom2dUtils.Position
import org.primetalk.advent.tools.Geom3dUtils.Vector3d
import org.primetalk.advent.tools.{Display, SequenceUtils, Utils}

import scala.reflect.ClassTag

/**
  *
  * https://adventofcode.com/2019/day/24
  *
  * --- Day 24: Planet of Discord ---
  *
  * You land on Eris, your last stop before reaching Santa. As soon as you do, your sensors start picking up strange life forms moving around: Eris is infested with bugs! With an over 24-hour roundtrip for messages between you and Earth, you'll have to deal with this problem on your own.
  *
  * Eris isn't a very large place; a scan of the entire area fits into a 5x5 grid (your puzzle input). The scan shows bugs (#) and empty spaces (.).
  *
  * Each minute, The bugs live and die based on the number of bugs in the four adjacent tiles:
  *
  * A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
  * An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
  *
  * Otherwise, a bug or empty space remains the same. (Tiles on the edges of the grid have fewer than four adjacent tiles; the missing tiles count as empty space.) This process happens in every location simultaneously; that is, within the same minute, the number of adjacent bugs is counted for every tile first, and then the tiles are updated.
  *
  * Here are the first few minutes of an example scenario:
  *
  * Initial state:
  * ....#
  * #..#.
  * #..##
  * ..#..
  * #....
  *
  * After 1 minute:
  * #..#.
  * ####.
  * ###.#
  * ##.##
  * .##..
  *
  * After 2 minutes:
  * #####
  * ....#
  * ....#
  * ...#.
  * #.###
  *
  * After 3 minutes:
  * #....
  * ####.
  * ...##
  * #.##.
  * .##.#
  *
  * After 4 minutes:
  * ####.
  * ....#
  * ##..#
  * .....
  * ##...
  *
  * To understand the nature of the bugs, watch for the first time a layout of bugs and empty spaces matches any previous layout. In the example above, the first layout to appear twice is:
  *
  * .....
  * .....
  * .....
  * #....
  * .#...
  *
  * To calculate the biodiversity rating for this layout, consider each tile left-to-right in the top row, then left-to-right in the second row, and so on. Each of these tiles is worth biodiversity points equal to increasing powers of two: 1, 2, 4, 8, 16, 32, and so on. Add up the biodiversity points for tiles with bugs; in this example, the 16th tile (32768 points) and 22nd tile (2097152 points) have bugs, a total biodiversity rating of 2129920.
  *
  * What is the biodiversity rating for the first layout that appears twice?
  *
  * Your puzzle answer was 18350099.
  * --- Part Two ---
  *
  * After careful analysis, one thing is certain: you have no idea where all these bugs are coming from.
  *
  * Then, you remember: Eris is an old Plutonian settlement! Clearly, the bugs are coming from recursively-folded space.
  *
  * This 5x5 grid is only one level in an infinite number of recursion levels. The tile in the middle of the grid is actually another 5x5 grid, the grid in your scan is contained as the middle tile of a larger 5x5 grid, and so on. Two levels of grids look like this:
  *
  * |     |         |     |
  * |     |         |     |
  * |     |         |     |
  * -----+-----+---------+-----+-----
  * |     |         |     |
  * |     |         |     |
  * |     |         |     |
  * -----+-----+---------+-----+-----
  * |     | | | | | |     |
  * |     |-+-+-+-+-|     |
  * |     | | | | | |     |
  * |     |-+-+-+-+-|     |
  * |     | | |?| | |     |
  * |     |-+-+-+-+-|     |
  * |     | | | | | |     |
  * |     |-+-+-+-+-|     |
  * |     | | | | | |     |
  * -----+-----+---------+-----+-----
  * |     |         |     |
  * |     |         |     |
  * |     |         |     |
  * -----+-----+---------+-----+-----
  * |     |         |     |
  * |     |         |     |
  * |     |         |     |
  *
  * (To save space, some of the tiles are not drawn to scale.) Remember, this is only a small part of the infinitely recursive grid; there is a 5x5 grid that contains this diagram, and a 5x5 grid that contains that one, and so on. Also, the ? in the diagram contains another 5x5 grid, which itself contains another 5x5 grid, and so on.
  *
  * The scan you took (your puzzle input) shows where the bugs are on a single level of this structure. The middle tile of your scan is empty to accommodate the recursive grids within it. Initially, no other levels contain bugs.
  *
  * Tiles still count as adjacent if they are directly up, down, left, or right of a given tile. Some tiles have adjacent tiles at a recursion level above or below its own level. For example:
  *
  * |     |         |     |
  * 1  |  2  |    3    |  4  |  5
  * |     |         |     |
  * -----+-----+---------+-----+-----
  * |     |         |     |
  * 6  |  7  |    8    |  9  |  10
  * |     |         |     |
  * -----+-----+---------+-----+-----
  * |     |A|B|C|D|E|     |
  * |     |-+-+-+-+-|     |
  * |     |F|G|H|I|J|     |
  * |     |-+-+-+-+-|     |
  * 11  | 12  |K|L|?|N|O|  14 |  15
  * |     |-+-+-+-+-|     |
  * |     |P|Q|R|S|T|     |
  * |     |-+-+-+-+-|     |
  * |     |U|V|W|X|Y|     |
  * -----+-----+---------+-----+-----
  * |     |         |     |
  * 16  | 17  |    18   |  19 |  20
  * |     |         |     |
  * -----+-----+---------+-----+-----
  * |     |         |     |
  * 21  | 22  |    23   |  24 |  25
  * |     |         |     |
  *
  * Tile 19 has four adjacent tiles: 14, 18, 20, and 24.
  * Tile G has four adjacent tiles: B, F, H, and L.
  * Tile D has four adjacent tiles: 8, C, E, and I.
  * Tile E has four adjacent tiles: 8, D, 14, and J.
  * Tile 14 has eight adjacent tiles: 9, E, J, O, T, Y, 15, and 19.
  * Tile N has eight adjacent tiles: I, O, S, and five tiles within the sub-grid marked ?.
  *
  * The rules about bugs living and dying are the same as before.
  *
  * For example, consider the same initial state as above:
  *
  * ....#
  * #..#.
  * #.?##
  * ..#..
  * #....
  *
  * The center tile is drawn as ? to indicate the next recursive grid. Call this level 0; the grid within this one is level 1, and the grid that contains this one is level -1. Then, after ten minutes, the grid at each level would look like this:
  *
  * Depth -5:
  * ..#..
  * .#.#.
  * ..?.#
  * .#.#.
  * ..#..
  *
  * Depth -4:
  * ...#.
  * ...##
  * ..?..
  * ...##
  * ...#.
  *
  * Depth -3:
  * #.#..
  * .#...
  * ..?..
  * .#...
  * #.#..
  *
  * Depth -2:
  * .#.##
  * ....#
  * ..?.#
  * ...##
  * .###.
  *
  * Depth -1:
  * #..##
  * ...##
  * ..?..
  * ...#.
  * .####
  *
  * Depth 0:
  * .#...
  * .#.##
  * .#?..
  * .....
  * .....
  *
  * Depth 1:
  * .##..
  * #..##
  * ..?.#
  * ##.##
  * #####
  *
  * Depth 2:
  * ###..
  * ##.#.
  * #.?..
  * .#.##
  * #.#..
  *
  * Depth 3:
  * ..###
  * .....
  * #.?..
  * #....
  * #...#
  *
  * Depth 4:
  * .###.
  * #..#.
  * #.?..
  * ##.#.
  * .....
  *
  * Depth 5:
  * ####.
  * #..#.
  * #.?#.
  * ####.
  * .....
  *
  * In this example, after 10 minutes, a total of 99 bugs are present.
  *
  * Starting with your scan, how many bugs are present after 200 minutes?
  *
  * Your puzzle answer was 2037.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day24 extends Utils {

  lazy val inputTextFromResource : Iterator[String] =
    readResource("day24.txt")
//    readResource("day24test2.txt")


  lazy val inputText: Seq[String] =
    inputTextFromResource.toSeq
  val initialDisplay: Display[Char] = Display.readCharDisplay(inputText)

  val masks: Map[Position, Int] = {
    initialDisplay.pointsLeftToRightTopToBottomYGrowsDown.foldLeft((Map[Position, Int](), 1)){
      case ((m, i), pos) => (m.updated(pos, i), i * 2)
    }._1
  }

  def biodiversityRating(d: Display[Char]): Int = {
    val res = d.pointsLeftToRightTopToBottomYGrowsDown.filter(p => d(p) == '#').map(masks).sum
    println(s"r:$res")
    res
  }

  def show(rating: Int): Display[Char] = {
    val d = Display.of((0,0), initialDisplay.size){ pos =>
      if((rating & masks(pos))  > 0)
        '#'
      else
        '.'
    }
    d
  }
  def rule(char: Char, bugCount: Int): Char = char match {
    case '#' =>
      if(bugCount == 1)
        '#'
      else
        '.'
    case _ =>
      if(bugCount == 1 || bugCount == 2)
        '#'
      else
        '.'
//    case _ => throw new IllegalArgumentException(s"Unexpected char $char")
  }
  def step(d: Display[Char]): Display[Char] = {
    println
    println(d.showDisplay()())
    val res = d.produceByLocalRulesFromMainDirections{
      case (c, nei) =>
        rule(c, nei.count(_ == '#'))
    }
    println("vvvvv")
    println(res.showDisplay()())
    res
  }

  lazy val answer1: Long = {
    val start = biodiversityRating(initialDisplay)
    val f: Int => Int = (show _).andThen(step).andThen(biodiversityRating)
    val (s, l) = SequenceUtils.floyd(start)()(f)
    (s + l)
  }

  // Part 2

  class Display3D[T:ClassTag](val offset3d: Vector3d, val size3d: Vector3d) {
    val inner: Display[T] = Display[T](convertPositionToInner(offset3d), (size3d._1 * size3d._3, size3d._2))()

    def writeLayer(z: Int, d: Display[T]): Unit = {
      d.points.foreach{ p =>
        update((p._1, p._2, z), d(p))
      }
    }

    def convertPositionToInner(p: Vector3d): Position = (p._1 + p._3 * size3d._1, p._2)

    def apply(p: Vector3d): T = inner.apply(convertPositionToInner(p))
    def update(p: Vector3d, v: T): Unit = inner.update(convertPositionToInner(p), v)
    def shallowCopy: Display3D[T] = {
      val res = new Display3D[T](offset3d, size3d)
      res.inner.renderFunction(p => inner(p))
      res
    }
    val minX: Int = offset3d._1
    val maxXplusExtra1: Int = minX + size3d._1
    val maxX: Int = maxXplusExtra1 - 1

    val minY: Int = offset3d._2
    val maxYplusExtra1: Int = minY + size3d._2
    val maxY: Int = maxYplusExtra1 - 1

    val minZ: Int = offset3d._3
    val maxZplusExtra1: Int = minZ + size3d._3
    val maxZ: Int = maxZplusExtra1 - 1

    def xs: List[Int] = (minX until maxXplusExtra1).toList
    def ys: List[Int] = (minY until maxYplusExtra1).toList
    def zs: List[Int] = (minZ until maxZplusExtra1).toList

    def points: List[Vector3d] =
      for{
        k <- zs
        j <- ys
        i <- xs
      } yield (i,j,k)

    def isWithinRange(p: Vector3d): Boolean =
      p._1 >= minX && p._1 <= maxX &&
      p._2 >= minY && p._2 <= maxY &&
      p._3 >= minZ && p._3 <= maxZ

    /** Draws the function on this display. */
    def renderFunction(f: Vector3d => T): Unit = {
      for{
        p <- points
      } {
        this(p) = f(p)
      }
    }

    def values: List[T] = points.map(apply)
  }

  def ifOnly[T](b: Boolean, lst: => List[T]): List[T] =
    if(b) lst else List()

  def neighboursMem[T](d: Display3D[T]): Map[Vector3d, List[Vector3d]] = {
    d.points.
      filterNot(p => p._1 == 2 && p._2 == 2).
      map( p => (p, neighbours(d)(p)) ).
      toMap
  }

  def neighbours[T](d: Display3D[T])(v: Vector3d): List[Vector3d] = v match {
    case (x,y,z) =>
      implicit class ListOps(lst: List[Vector3d]){
        def inRange: List[Vector3d] = {
          val out = lst.filterNot(d.isWithinRange)
          if(out.nonEmpty) throw new IllegalArgumentException(s"Some elements are outside the range: $out")
          lst
        }
      }
      List(
        ifOnly(x == d.minX && z < d.maxZ, List((1,2,z + 1)).inRange),
        ifOnly(x > d.minX && !(x == 3 && y == 2), List((x - 1, y, z)).inRange),
        ifOnly(x == 3 && y == 2 && z > d.minZ, d.ys.map{yy =>(d.maxX, yy, z - 1)}.inRange),

        ifOnly(x == d.maxX && z < d.maxZ, List((3,2,z + 1)).inRange),
        ifOnly(x < d.maxX && !(x == 1 && y == 2), List((x + 1, y, z)).inRange),
        ifOnly(x == 1 && y == 2 && z > d.minZ, d.ys.map{yy =>(d.minX, yy, z - 1)}.inRange),

        ifOnly(y == d.minY && z < d.maxZ, List((2,1,z + 1)).inRange),
        ifOnly(y > d.minY && !(x == 2 && y == 3), List((x, y - 1, z)).inRange),
        ifOnly(x == 2 && y == 3 && z > d.minZ, d.xs.map{xx =>(xx, d.maxY, z - 1)}.inRange),

        ifOnly(y == d.maxY && z < d.maxZ, List((2,3,z + 1)).inRange),
        ifOnly(y < d.maxY && !(x == 2 && y == 1), List((x, y + 1, z)).inRange),
        ifOnly(x == 2 && y == 1 && z > d.minZ, d.xs.map{xx =>(xx, d.minY, z - 1)}.inRange),
        Nil
      ).flatten
  }

  def step3d(nei: Map[Vector3d, List[Vector3d]])(d: Display3D[Char]): Display3D[Char] = {
//    println()
//    println(d.inner.showDisplay()())
    val res = new Display3D[Char](d.offset3d, d.size3d)
    res.renderFunction{ p =>
      if(p._1 == 2 && p._2 == 2)
        '?'
      else {
        val n = nei(p)
        val cnt = n.count(pp => d(pp) == '#')
        val nextChar = rule(d(p), cnt)
        nextChar
      }
    }
    println("vvvvv")
//    println(res.inner.showDisplay()())
    res
  }

  lazy val answer2: Int = {
    val epochs = 200
    val d = new Display3D[Char]((0,0,-epochs), (5,5,epochs * 2 + 1))
    println(d.points.filterNot(d.isWithinRange))
    d.writeLayer(0, initialDisplay)
    val mem = neighboursMem(d)
    require(mem((3,3,1)).size == 4)
    require(mem((1,1,0)).size == 4)
    require(mem((3,1,0)).size == 4)
    require(mem((4,1,0)).size == 4)
    require(mem((3,2,1)).size == 8)
    require(mem((3,2,0)).size == 8)
    val last = SequenceUtils.unfoldN(d, epochs)(step3d(mem))
    last.values.count(_ == '#')

  }

  def main(args: Array[String]): Unit = {
//    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }
}
