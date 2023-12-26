package org.primetalk.advent2023

import org.primetalk.advent3.tools.{GraphUtils, IDisplay2D, MyPriorityQueue, Priority, Utils}
import org.primetalk.advent3.tools.Geom2dUtils.{*, given}
import org.primetalk.advent3.tools.GraphUtils.{PartialSearchResult, PartialSearchResultWithOrdering, PartialSearchResultWithPriority, PartialSearchResultWithPriority2}
import cats.kernel.Order
import cats.collections.Heap

/**
  * https://adventofcode.com/2023/day/17
--- Day 17: Clumsy Crucible ---

The lava starts flowing rapidly once the Lava Production Facility is operational. As you leave, the reindeer offers you a parachute, allowing you to quickly reach Gear Island.

As you descend, your bird's-eye view of Gear Island reveals why you had trouble finding anyone on your way up: half of Gear Island is empty, but the half below you is a giant factory city!

You land near the gradually-filling pool of lava at the base of your new lavafall. Lavaducts will eventually carry the lava throughout the city, but to make use of it immediately, Elves are loading it into large crucibles on wheels.

The crucibles are top-heavy and pushed by hand. Unfortunately, the crucibles become very difficult to steer at high speeds, and so it can be hard to go in a straight line for very long.

To get Desert Island the machine parts it needs as soon as possible, you'll need to find the best way to get the crucible from the lava pool to the machine parts factory. To do this, you need to minimize heat loss while choosing a route that doesn't require the crucible to go in a straight line for too long.

Fortunately, the Elves here have a map (your puzzle input) that uses traffic patterns, ambient temperature, and hundreds of other parameters to calculate exactly how much heat loss can be expected for a crucible entering any particular city block.

For example:

2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533

Each city block is marked by a single digit that represents the amount of heat loss if the crucible enters that block. The starting point, the lava pool, is the top-left city block; the destination, the machine parts factory, is the bottom-right city block. (Because you already start in the top-left block, you don't incur that block's heat loss unless you leave that block and then return to it.)

Because it is difficult to keep the top-heavy crucible going in a straight line for very long, it can move at most three blocks in a single direction before it must turn 90 degrees left or right. The crucible also can't reverse direction; after entering each city block, it may only turn left, continue straight, or turn right.

One way to minimize heat loss is this path:

2>>34^>>>1323
32v>>>35v5623
32552456v>>54
3446585845v52
4546657867v>6
14385987984v4
44578769877v6
36378779796v>
465496798688v
456467998645v
12246868655<v
25465488877v5
43226746555v>

This path never moves more than three consecutive blocks in the same direction and incurs a heat loss of only 102.

Directing the crucible from the lava pool to the machine parts factory, but not moving more than three consecutive blocks in the same direction, what is the least heat loss it can incur?

Your puzzle answer was 758.
--- Part Two ---

The crucibles of lava simply aren't large enough to provide an adequate supply of lava to the machine parts factory. Instead, the Elves are going to upgrade to ultra crucibles.

Ultra crucibles are even more difficult to steer than normal crucibles. Not only do they have trouble going in a straight line, but they also have trouble turning!

Once an ultra crucible starts moving in a direction, it needs to move a minimum of four blocks in that direction before it can turn (or even before it can stop at the end). However, it will eventually start to get wobbly: an ultra crucible can move a maximum of ten consecutive blocks without turning.

In the above example, an ultra crucible could follow this path to minimize heat loss:

2>>>>>>>>1323
32154535v5623
32552456v4254
34465858v5452
45466578v>>>>
143859879845v
445787698776v
363787797965v
465496798688v
456467998645v
122468686556v
254654888773v
432267465553v

In the above example, an ultra crucible would incur the minimum possible heat loss of 94.

Here's another example:

111111111111
999999999991
999999999991
999999999991
999999999991

Sadly, an ultra crucible would need to take an unfortunate path like this one:

1>>>>>>>1111
9999999v9991
9999999v9991
9999999v9991
9999999v>>>>

This route causes the ultra crucible to incur the minimum possible heat loss of 71.

Directing the ultra crucible from the lava pool to the machine parts factory, what is the least heat loss it can incur?

Your puzzle answer was 892.

Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2317 extends Utils:

  val input: String = readThisObjectInput

  val maze = IDisplay2D.readCharDisplayFromString(input, ' ')

  case class State(p: Position, dir: Direction, straightCount: Int)
  case class StateWithHeatLoss(s: State, heatLoss: Int)

  def next(s: State): List[StateWithHeatLoss] =
    import s.*
    val angles =
    straightCount match
      case 3 => List(90, 270)
      case _ => List(0, 90, 270)

    for
      angle <- angles
      newDir = dir.rotateByDegree(angle)
      position = p + newDir
      if maze.isWithinRange(position)
      nextCount = if angle == 0 then straightCount + 1 else 1
    yield
      StateWithHeatLoss(State(position, newDir, nextCount), maze(position) - '0')

  // 758
  lazy val answer1: Int =
    case class PathInfo[T](totalHeatLoss: Int, head: T):
      inline def node: T = head
      def prepend(h: T, heatLoss: Int): PathInfo[T] =
        PathInfo(totalHeatLoss + heatLoss, h)

    val initialPaths = mainDirections.map(dir => PathInfo(0, State(maze.rect.topLeft, dir, 0)))

    given Ordering[PathInfo[State]] = Ordering.by(_.totalHeatLoss)

    given Priority[PathInfo[State]] with
      def apply(p: PathInfo[State]): Long = p.totalHeatLoss

    val found = GraphUtils.priorityFindFirst[PathInfo[State], PathInfo[State], Map[State, PathInfo[State]]](
      map => path => {
        val key = path.node
        if map.contains(key) && map(key).totalHeatLoss < path.totalHeatLoss then
          PartialSearchResultWithPriority(MyPriorityQueue.empty, Nil)
        else
          val nextPoints = next(key)
          val nextPaths = nextPoints.map { np => path.prepend(np.s, np.heatLoss) }
          val (found, notYet) = nextPaths.partition(np => np.node.p == maze.rect.bottomRight)
          PartialSearchResultWithPriority(MyPriorityQueue.fromList(notYet), found)
      },
      (map: Map[State, PathInfo[State]], q: MyPriorityQueue[PathInfo[State]]) =>
        val (m, sortedBackwards) = q.sorted.foldLeft((map, List[PathInfo[State]]())) {
          case ((m, lst), path) =>
            val key = path.node
            if m.contains(key) && m(key).totalHeatLoss <= path.totalHeatLoss then
              (m, lst)
            else
            // println(s"$key: ${path.totalHeatLoss}")
              (m.updated(key, path), path :: lst)
        }
        (m, MyPriorityQueue(sortedBackwards.reverse))
    )(initialPaths.map(ip => ip.node -> ip).toMap, MyPriorityQueue(initialPaths), Nil)
    found.head.totalHeatLoss
  
  //Part 2

  def next2(s: State): List[StateWithHeatLoss] =
    import s.*
    val angles =
      straightCount match
        case 10 => List(90, 270)
        case _  => List(0, 90, 270)

    for
      angle <- angles
      newDir = dir.rotateByDegree(angle)
      mult = if angle == 0 then 1 else 4
      nextStraightCount = mult + (if angle == 0 then straightCount else 0)
      newPosition = p + newDir * mult
      if maze.isWithinRange(newPosition)
      heatLoss = (1 to mult)
        .map(i => p + newDir * i)
        .map(p => maze(p) - '0').sum
    yield
      StateWithHeatLoss(State(newPosition, newDir, nextStraightCount), heatLoss)

  // 925, 895 - too high 895,
  lazy val answer20: Int =
    case class PathInfo[T](totalHeatLoss: Int, head: T):
      inline def node: T = head

      def prepend(h: T, heatLoss: Int): PathInfo[T] =
        PathInfo(totalHeatLoss + heatLoss, h)

    val initialPaths = mainDirections.map(dir => PathInfo(0, State(maze.rect.topLeft, dir, 10)))

    given Ordering[PathInfo[State]] = Ordering.by(_.totalHeatLoss)

    given Priority[PathInfo[State]] with
      def apply(p: PathInfo[State]): Long = p.totalHeatLoss

    val found = GraphUtils.priorityFindFirst[PathInfo[State], PathInfo[State], Map[State, PathInfo[State]]](
      map => path => {
        val key = path.node
        if map.contains(key) && map(key).totalHeatLoss < path.totalHeatLoss then
          PartialSearchResultWithPriority(MyPriorityQueue.empty, Nil)
        else
          val nextPoints = next2(key)
          val nextPaths = nextPoints.map { np => path.prepend(np.s, np.heatLoss) }
          val (found, notYet) = nextPaths.partition(np => np.node.p == maze.rect.bottomRight)
          PartialSearchResultWithPriority(MyPriorityQueue.fromList(notYet), found)
      },
      (map: Map[State, PathInfo[State]], q: MyPriorityQueue[PathInfo[State]]) =>
        val (m, sortedBackwards) = q.toList.foldLeft((map, List[PathInfo[State]]())) {
          case ((m, lst), path) =>
            val key = path.node
            if m.contains(key) && m(key).totalHeatLoss <= path.totalHeatLoss then
              (m, lst)
            else
            // println(s"$key: ${path.totalHeatLoss}")
              (m.updated(key, path), path :: lst)
        }
        (m, MyPriorityQueue(sortedBackwards.reverse))
    )(initialPaths.map(ip => ip.node -> ip).toMap, MyPriorityQueue(initialPaths), Nil)
    found.head.totalHeatLoss

  // 886, 895, 920?? 892
  lazy val answer2: Int =
    case class PathInfo[T](totalHeatLoss: Int, head: T):
      inline def node: T = head

      def prepend(h: T, heatLoss: Int): PathInfo[T] =
        PathInfo(totalHeatLoss + heatLoss, h)

    val initialPaths = mainDirections.map(dir => PathInfo(0, State(maze.rect.topLeft, dir, 10)))

    given Order[PathInfo[State]] = Order.by(_.totalHeatLoss)

    case class GlobalState(m: Map[State, PathInfo[State]] = Map(), limit: Int = Int.MaxValue)

    val found = GraphUtils.priorityFindAll2[PathInfo[State], PathInfo[State], GlobalState](
      path => {
        val key = path.node
        val nextPoints = next2(key)
        val nextPaths = nextPoints.map { np => path.prepend(np.s, np.heatLoss) }
        val (found, notYet) = nextPaths.partition(_.node.p == maze.rect.bottomRight)
        PartialSearchResultWithPriority2(notYet, found)
      },
      (m, found) => m.copy(limit = found.foldLeft(m.limit)((l, pathInfo) => math.min(l,pathInfo.totalHeatLoss))),
      (gs: GlobalState, i: Iterable[PathInfo[State]]) =>
        val (gs2, sortedBackwards) = i.foldLeft((gs, List[PathInfo[State]]())) {
          case ((m, lst), path) =>
            val key = path.node
            if gs.limit < path.totalHeatLoss || gs.m.get(key).exists(_.totalHeatLoss <= path.totalHeatLoss) then
              (m, lst)
            else
              (gs.copy(m = gs.m.updated(key, path)), path :: lst)
        }
        (gs2, sortedBackwards)
    )(GlobalState(), Heap.fromIterable(initialPaths), Nil)
    found.minBy(_.totalHeatLoss).totalHeatLoss

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
