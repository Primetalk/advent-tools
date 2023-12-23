package org.primetalk.advent2023

import cats.collections.Heap
import cats.kernel.Order
import org.primetalk.advent3.tools.{GraphUtils, IDisplay2D, Utils}
import org.primetalk.advent3.tools.Geom2dUtils.{*, given}
import org.primetalk.advent3.tools.GraphUtils.PartialSearchResultWithPriority2
/**
  * https://adventofcode.com/2023/day/23
--- Day 23: A Long Walk ---

The Elves resume water filtering operations! Clean water starts flowing over the edge of Island Island.

They offer to help you go over the edge of Island Island, too! Just hold on tight to one end of this impossibly long rope and they'll lower you down a safe distance from the massive waterfall you just created.

As you finally reach Snow Island, you see that the water isn't really reaching the ground: it's being absorbed by the air itself. It looks like you'll finally have a little downtime while the moisture builds up to snow-producing levels. Snow Island is pretty scenic, even without any snow; why not take a walk?

There's a map of nearby hiking trails (your puzzle input) that indicates paths (.), forest (#), and steep slopes (^, >, v, and <).

For example:

#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#

You're currently on the single path tile in the top row; your goal is to reach the single path tile in the bottom row. Because of all the mist from the waterfall, the slopes are probably quite icy; if you step onto a slope tile, your next step must be downhill (in the direction the arrow is pointing). To make sure you have the most scenic hike possible, never step onto the same tile twice. What is the longest hike you can take?

In the example above, the longest hike you can take is marked with O, and your starting position is marked S:

#S#####################
#OOOOOOO#########...###
#######O#########.#.###
###OOOOO#OOO>.###.#.###
###O#####O#O#.###.#.###
###OOOOO#O#O#.....#...#
###v###O#O#O#########.#
###...#O#O#OOOOOOO#...#
#####.#O#O#######O#.###
#.....#O#O#OOOOOOO#...#
#.#####O#O#O#########v#
#.#...#OOO#OOO###OOOOO#
#.#.#v#######O###O###O#
#...#.>.#...>OOO#O###O#
#####v#.#.###v#O#O###O#
#.....#...#...#O#O#OOO#
#.#########.###O#O#O###
#...###...#...#OOO#O###
###.###.#.###v#####O###
#...#...#.#.>.>.#.>O###
#.###.###.#.###.#.#O###
#.....###...###...#OOO#
#####################O#

This hike contains 94 steps. (The other possible hikes you could have taken were 90, 86, 82, 82, and 74 steps long.)

Find the longest hike you can take through the hiking trails listed on your map. How many steps long is the longest hike?

Your puzzle answer was 2178.
--- Part Two ---

As you reach the trailhead, you realize that the ground isn't as slippery as you expected; you'll have no problem climbing up the steep slopes.

Now, treat all slopes as if they were normal paths (.). You still want to make sure you have the most scenic hike possible, so continue to ensure that you never step onto the same tile twice. What is the longest hike you can take?

In the example above, this increases the longest hike to 154 steps:

#S#####################
#OOOOOOO#########OOO###
#######O#########O#O###
###OOOOO#.>OOO###O#O###
###O#####.#O#O###O#O###
###O>...#.#O#OOOOO#OOO#
###O###.#.#O#########O#
###OOO#.#.#OOOOOOO#OOO#
#####O#.#.#######O#O###
#OOOOO#.#.#OOOOOOO#OOO#
#O#####.#.#O#########O#
#O#OOO#...#OOO###...>O#
#O#O#O#######O###.###O#
#OOO#O>.#...>O>.#.###O#
#####O#.#.###O#.#.###O#
#OOOOO#...#OOO#.#.#OOO#
#O#########O###.#.#O###
#OOO###OOO#OOO#...#O###
###O###O#O###O#####O###
#OOO#OOO#O#OOO>.#.>O###
#O###O###O#O###.#.#O###
#OOOOO###OOO###...#OOO#
#####################O#

Find the longest hike you can take through the surprisingly dry hiking trails listed on your map. How many steps long is the longest hike?

Your puzzle answer was 6486.

Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2323 extends Utils:

  val input: String = readThisObjectInput

  val maze = IDisplay2D.readCharDisplayFromString(input, '.')
  val startingPosition = maze
    .edgePositionsByDirClockwise(Up).find(maze(_) == '.')
    .getOrElse(throw IllegalArgumentException("start not found"))
  val finishingPosition = maze
    .edgePositionsByDirClockwise(Down).find(maze(_) == '.')
    .getOrElse(throw IllegalArgumentException("finish not found"))
  case class State(position: Position, visited: Set[Position], length: Int):
    def advance(p1: Position): State =
      State(p1, visited + p1, length + 1)

  def allowedDirections(position: Position): List[Direction] = maze(position) match
    case '>' => List(Right)
    case '<' => List(Left)
    case 'v' => List(Up)
    case '^' => List(Down)
    case '.' => mainDirections
  def nextP(allowedDirections: Position => List[Direction])(position: Position): List[Position] =
    allowedDirections(position)
      .map(dir => position + dir)
      .filter(maze.isWithinRange)
      .filterNot(p => maze(p) == '#')

  def next(allowedDirections: Position => List[Direction])(s: State): List[State] =
    import s.*
    allowedDirections(position)
      .map(dir => position + dir)
      .filter(maze.isWithinRange)
      .filterNot(p => maze(p) == '#')
      .filterNot(visited.contains)
      .map(s.advance)
  def isFinal(s: State): Boolean =
    s.position._2 == maze.maxY

  val start = State(startingPosition, Set(startingPosition), 0)
  // 1810 -> 2178
  lazy val answer1: Int =

    given Order[State] = Order.by(gs => -gs.length)
    val nextA = next(allowedDirections)
    val found = GraphUtils.priorityFindAll2[State, State, Long](
      key => {
        val nextPoints = nextA(key)
        val (found, notYet) = nextPoints.partition(np => isFinal(np))
        PartialSearchResultWithPriority2(notYet, found)
      },
      (limit, found) =>
        if found.isEmpty then
          limit
        else {
          val newLimit = math.max(found.map(_.length).max, limit)
          if newLimit > limit then println(s"limit: $newLimit")
          newLimit
        },

      (limit, alternatives) =>
        if limit == Long.MinValue then
          (limit, alternatives)
        else
          (limit, alternatives)//.filter(_.length >= limit))
    )(Long.MinValue, Heap(start), Nil)
    val fnd = found.sortBy(-_.length)

    fnd.head.length

  //Part 2 // 6290, 4798 - too low; 6486
  lazy val answer2: Int =
    val nextA = nextP(_ => mainDirections)
    val map = GraphUtils.simplifyGraph(nextA, List(startingPosition, finishingPosition))
    println(map.mkString("\n"))

    given Order[State] = Order.by(gs => -gs.length)
    val found = GraphUtils.priorityFindAll2[State, State, Long](
      key => {
        val nextPoints = map(key.position).toList.filterNot{ case (p,l) => key.visited.contains(p) }
        val nextStates = nextPoints.map{ case (p, l) => State(p, key.visited + key.position, key.length + l)}
        val (found, notYet) = nextStates.partition(np => isFinal(np))
        PartialSearchResultWithPriority2(notYet, found)
      },
      (limit, found) =>
        if found.isEmpty then
          limit
        else {
          val newLimit = math.max(found.map(_.length).max, limit)
          if newLimit > limit then println(s"limit: $newLimit")
          newLimit
        },
      (limit, alternatives) => (limit, alternatives)
    )(Long.MinValue, Heap(start), Nil)
    val fnd = found.sortBy(-_.length)

    fnd.head.length

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
