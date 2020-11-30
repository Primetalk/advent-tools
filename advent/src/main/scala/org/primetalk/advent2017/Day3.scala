package org.primetalk.advent2017

import org.primetalk.advent.tools.Display
import org.primetalk.advent.tools.Geom2dUtils.{Direction, Down, Left, PosOps, Position, Right, Up, Vector2d, manhattanDistance, mul}
import org.primetalk.advent2018.ProgUtils

/**
  * --- Day 3: Spiral Memory ---
  *
  * You come across an experimental new kind of memory stored on an infinite two-dimensional grid.
  *
  * Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:
  *
  * 17  16  15  14  13
  * 18   5   4   3  12
  * 19   6   1   2  11
  * 20   7   8   9  10
  * 21  22  23---> ...
  *
  * While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.
  *
  * For example:
  *
  * Data from square 1 is carried 0 steps, since it's at the access port.
  * Data from square 12 is carried 3 steps, such as: down, left, left.
  * Data from square 23 is carried only 2 steps: up twice.
  * Data from square 1024 must be carried 31 steps.
  *
  * How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?
  *
  * Your puzzle answer was 430.
  * --- Part Two ---
  *
  * As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.
  *
  * So, the first few squares' values are chosen as follows:
  *
  * Square 1 starts with the value 1.
  * Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
  * Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
  * Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
  * Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
  *
  * Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:
  *
  * 147  142  133  122   59
  * 304    5    4    2   57
  * 330   10    1    1   54
  * 351   11   23   25   26
  * 362  747  806--->   ...
  *
  * What is the first value written that is larger than your puzzle input?
  *
  * Your puzzle answer was 312453.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  *
  * At this point, you should return to your advent calendar and try another puzzle.
  *
  * Your puzzle input was 312051.
  */
object Day3 extends ProgUtils {
  case class State(pos: Position, direction: Direction, len: Int, leftSteps: Int, index: Int, indexInLine: Int)

  def initialState(n: Int): State = State((0, 0), Right, 1, n - 1, 1, 0)

  def jumpEdge: State => State = {
    case State(pos, direction, len, leftSteps, index, indexInLine) =>
      if(leftSteps <= len) {
        State(pos + mul(leftSteps)(direction), direction, len, 0, index + leftSteps, indexInLine + leftSteps)
      } else {
        State(pos + mul(len)(direction), direction, len, leftSteps - len, index + len, indexInLine + len)
      }
  }

  def rotate90: State => State = {
    case State(pos, Right, len, leftSteps, index, _) => State(pos, Up   , len    , leftSteps, index, 0)
    case State(pos, Up   , len, leftSteps, index, _) => State(pos, Left , len + 1, leftSteps, index, 0)
    case State(pos, Left , len, leftSteps, index, _) => State(pos, Down , len    , leftSteps, index, 0)
    case State(pos, Down , len, leftSteps, index, _) => State(pos, Right, len + 1, leftSteps, index, 0)
    case s@State(_, dir, _, _, _, _) => throw new IllegalArgumentException("Unknown direction " + dir + " in state " + s)
  }

  def spiralProgram: LazyList[Operation] =
    infiniteProgram(LazyList(jumpEdge, rotate90))

  val input: Int = 312051

  def findUltimatePosition(n: Int): Position = {
    findFirst(spiralProgram, _.leftSteps == 0).apply(initialState(n)).getOrElse(throw new IllegalArgumentException("not found")).pos
  }

  lazy val answer1: Int =
    manhattanDistance((0,0), findUltimatePosition(input))

  // Part 2

  /** Stepping one cell at a time. Performs rotation if needed.*/
  def smallStep: State => State = {
    case s@State(pos, direction, len, leftSteps, index, indexInLine) =>
      if(indexInLine < len) {
        State(pos + direction, direction, len, leftSteps - 1, index + 1, indexInLine + 1)
      } else {
        smallStep(rotate90(s))
      }
  }

  val maxSize = 100
  val offsetX: Int = -maxSize / 2
  val offsetY: Int = -maxSize / 2
  val offset: Vector2d = (offsetX, offsetY)
  val displaySize: Vector2d = (maxSize, maxSize)


//  type Display = Array[Array[Int]]

  def newDisplay(size: Vector2d): Display[Int] =
    Display((-size._1/2, -size._2/2), size)()

  def fillIn(input: Int): Int = {
    val display = newDisplay(displaySize)
    val init = initialState(input)
    display.update(init.pos, 1)
    def updatePosition(s: State): Int = {
      val v = display.inclusiveRectSum(s.pos + (-1, -1), s.pos + (1, 1))
      display.update(s.pos, v)
      v
    }
    def loop(s: State): Int = {
      val v: Int = updatePosition(s)
      if(v > input)
        v
      else
        loop(smallStep(s))
    }
    loop(init)
  }

  lazy val answer2: Int = {
    fillIn(input)
  }
}
