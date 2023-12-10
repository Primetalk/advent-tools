package org.primetalk.advent2022

import org.primetalk.advent3.tools.Utils
import org.primetalk.advent3.tools.Geom2dUtils._
import org.primetalk.advent3.tools.Display2D.apply
import org.primetalk.advent3.tools.Display2D
import scala.collection.immutable.Queue
import org.primetalk.advent3.tools.PrimeNumbers
import org.primetalk.advent3.tools.NumberSequenceUtils

/**
  * https://adventofcode.com/2022/day/17
--- Day 17: Pyroclastic Flow ---

Your handheld device has located an alternative exit from the cave for you and the elephants. The ground is rumbling almost continuously now, but the strange valves bought you some time. It's definitely getting warmer in here, though.

The tunnels eventually open into a very tall, narrow chamber. Large, oddly-shaped rocks are falling into the chamber from above, presumably due to all the rumbling. If you can't work out where the rocks will fall next, you might be crushed!

The five types of rocks have the following peculiar shapes, where # is rock and . is empty space:

####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##

The rocks fall in the order shown above: first the - shape, then the + shape, and so on. Once the end of the list is reached, the same order repeats: the - shape falls first, sixth, 11th, 16th, etc.

The rocks don't spin, but they do get pushed around by jets of hot gas coming out of the walls themselves. A quick scan reveals the effect the jets of hot gas will have on the rocks as they fall (your puzzle input).

For example, suppose this was the jet pattern in your cave:

>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>

In jet patterns, < means a push to the left, while > means a push to the right. The pattern above means that the jets will push a falling rock right, then right, then right, then left, then left, then right, and so on. If the end of the list is reached, it repeats.

The tall, vertical chamber is exactly seven units wide. Each rock appears so that its left edge is two units away from the left wall and its bottom edge is three units above the highest rock in the room (or the floor, if there isn't one).

After a rock appears, it alternates between being pushed by a jet of hot gas one unit (in the direction indicated by the next symbol in the jet pattern) and then falling one unit down. If any movement would cause any part of the rock to move into the walls, floor, or a stopped rock, the movement instead does not occur. If a downward movement would have caused a falling rock to move into the floor or an already-fallen rock, the falling rock stops where it is (having landed on something) and a new rock immediately begins falling.

Drawing falling rocks with @ and stopped rocks with #, the jet pattern in the example above manifests as follows:

The first rock begins falling:
|..@@@@.|
|.......|
|.......|
|.......|
+-------+

Jet of gas pushes rock right:
|...@@@@|
|.......|
|.......|
|.......|
+-------+

Rock falls 1 unit:
|...@@@@|
|.......|
|.......|
+-------+

Jet of gas pushes rock right, but nothing happens:
|...@@@@|
|.......|
|.......|
+-------+

Rock falls 1 unit:
|...@@@@|
|.......|
+-------+

Jet of gas pushes rock right, but nothing happens:
|...@@@@|
|.......|
+-------+

Rock falls 1 unit:
|...@@@@|
+-------+

Jet of gas pushes rock left:
|..@@@@.|
+-------+

Rock falls 1 unit, causing it to come to rest:
|..####.|
+-------+

A new rock begins falling:
|...@...|
|..@@@..|
|...@...|
|.......|
|.......|
|.......|
|..####.|
+-------+

Jet of gas pushes rock left:
|..@....|
|.@@@...|
|..@....|
|.......|
|.......|
|.......|
|..####.|
+-------+

Rock falls 1 unit:
|..@....|
|.@@@...|
|..@....|
|.......|
|.......|
|..####.|
+-------+

Jet of gas pushes rock right:
|...@...|
|..@@@..|
|...@...|
|.......|
|.......|
|..####.|
+-------+

Rock falls 1 unit:
|...@...|
|..@@@..|
|...@...|
|.......|
|..####.|
+-------+

Jet of gas pushes rock left:
|..@....|
|.@@@...|
|..@....|
|.......|
|..####.|
+-------+

Rock falls 1 unit:
|..@....|
|.@@@...|
|..@....|
|..####.|
+-------+

Jet of gas pushes rock right:
|...@...|
|..@@@..|
|...@...|
|..####.|
+-------+

Rock falls 1 unit, causing it to come to rest:
|...#...|
|..###..|
|...#...|
|..####.|
+-------+

A new rock begins falling:
|....@..|
|....@..|
|..@@@..|
|.......|
|.......|
|.......|
|...#...|
|..###..|
|...#...|
|..####.|
+-------+

The moment each of the next few rocks begins falling, you would see this:

|..@....|
|..@....|
|..@....|
|..@....|
|.......|
|.......|
|.......|
|..#....|
|..#....|
|####...|
|..###..|
|...#...|
|..####.|
+-------+

|..@@...|
|..@@...|
|.......|
|.......|
|.......|
|....#..|
|..#.#..|
|..#.#..|
|#####..|
|..###..|
|...#...|
|..####.|
+-------+

|..@@@@.|
|.......|
|.......|
|.......|
|....##.|
|....##.|
|....#..|
|..#.#..|
|..#.#..|
|#####..|
|..###..|
|...#...|
|..####.|
+-------+

|...@...|
|..@@@..|
|...@...|
|.......|
|.......|
|.......|
|.####..|
|....##.|
|....##.|
|....#..|
|..#.#..|
|..#.#..|
|#####..|
|..###..|
|...#...|
|..####.|
+-------+

|....@..|
|....@..|
|..@@@..|
|.......|
|.......|
|.......|
|..#....|
|.###...|
|..#....|
|.####..|
|....##.|
|....##.|
|....#..|
|..#.#..|
|..#.#..|
|#####..|
|..###..|
|...#...|
|..####.|
+-------+

|..@....|
|..@....|
|..@....|
|..@....|
|.......|
|.......|
|.......|
|.....#.|
|.....#.|
|..####.|
|.###...|
|..#....|
|.####..|
|....##.|
|....##.|
|....#..|
|..#.#..|
|..#.#..|
|#####..|
|..###..|
|...#...|
|..####.|
+-------+

|..@@...|
|..@@...|
|.......|
|.......|
|.......|
|....#..|
|....#..|
|....##.|
|....##.|
|..####.|
|.###...|
|..#....|
|.####..|
|....##.|
|....##.|
|....#..|
|..#.#..|
|..#.#..|
|#####..|
|..###..|
|...#...|
|..####.|
+-------+

|..@@@@.|
|.......|
|.......|
|.......|
|....#..|
|....#..|
|....##.|
|##..##.|
|######.|
|.###...|
|..#....|
|.####..|
|....##.|
|....##.|
|....#..|
|..#.#..|
|..#.#..|
|#####..|
|..###..|
|...#...|
|..####.|
+-------+

To prove to the elephants your simulation is accurate, they want to know how tall the tower will get after 2022 rocks have stopped (but before the 2023rd rock begins falling). In this example, the tower of rocks will be 3068 units tall.

How many units tall will the tower of rocks be after 2022 rocks have stopped falling?

Your puzzle answer was 3157.
--- Part Two ---

The elephants are not impressed by your simulation. They demand to know how tall the tower will be after 1000000000000 rocks have stopped! Only then will they feel confident enough to proceed through the cave.

In the example above, the tower would be 1514285714288 units tall!

How tall will the tower be after 1000000000000 rocks have stopped?

Your puzzle answer was 1581449275319.

Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2217 extends Utils:

  val wind = Queue(readThisObjectInput.trim.toList:_*)

  val `s_` = "####"
  val `+` = """.#.
              |###
              |.#.""".stripMargin
  val L = """..#
            |..#
            |###""".stripMargin
  val `|` = """#
              |#
              |#
              |#""".stripMargin
  val `#` = """##
              |##""".stripMargin

  val shapeSequence = List(`s_`, `+`, L, `|`, `#`)
    .map(l => Display2D.readCharDisplay(l.split("\n").toSeq, '.').flipY)
  
  type Sprite = Display2D[Char]

  val spritesQueue = Queue(shapeSequence:_*)

  // val maxSize = 2022*13/5 + 1000
  val maxSize = 120000*13/5
  def display = Display2D[Char](origin, (7, maxSize))()

  case class Shape(kind: Sprite, position: Position):
    def move(dir: Direction): Shape = 
      this.copy(position = position + dir)
    def visiblePoints = 
      kind.findAll(_ == '#')
    def absolutePoints =
      visiblePoints.map(position + _)

  def isOverlapping(display: Display2D[Char], shape: Shape): Boolean =
    shape.absolutePoints.exists{displayCoords => 
      !display.isWithinRange(displayCoords) ||
        display(displayCoords) == '#'
    }

  val windMod = wind.length
  val spriteMod = spritesQueue.length

  case class Index(windIndex: Int, spriteIndex: Int):
    def incWind = Index((windIndex + 1)%windMod, spriteIndex)
    def incSprite = Index(windIndex, (spriteIndex + 1)%spriteMod)

  def play1(display: Display2D[Char], shape: Shape, wind: Queue[Char], index: Index, steps: Int = 0): (Shape, Queue[Char], Index) = 

    val (w, wind2) = wind.dequeue
    val wind3 = wind2.enqueue(w)
    val windIndex3 = index.incWind
    val dir = w match 
      case '<' => Left
      case '>' => Right
      case _ => ???
    val h = shape.move(dir)
    if isOverlapping(display, h) then
      val d = shape.move(Down)
      if isOverlapping(display, d) then
        (shape, wind3, windIndex3)
      else
        play1(display, d, wind3, windIndex3, steps + 1)
    else
      val d = h.move(Down)
      if isOverlapping(display, d) then
        (h, wind3, windIndex3)
      else
        play1(display, d, wind3, windIndex3, steps + 1)

  def printNshape(display: Display2D[Char], shape: Shape, n: Int): Unit =
    val copy = Display2D.fromIDisplay2D(display.toIDisplay2D)
    draw(copy, shape, '@')
    println("-")
    println{
      val s = copy.showDisplay()()
      val lines = s.split("\n")
      lines.take(n).map("|"+_+"|").zipWithIndex.map((l,i) => s"$l $i").reverse.mkString("\n")
    }
    println("+-------+")

  def print10(display: Display2D[Char]): Unit =
    println{
      val s = display.showDisplay()()
      val lines = s.split("\n")
      lines.take(10).mkString("\n")
    }

  def draw(display: Display2D[Char], shape: Shape, char: Char = '#'): Int =
    display(shape.absolutePoints) = char
    shape.absolutePoints.map(_._2).max + 1

  def spawn(height: Int, shapesQueue: Queue[Sprite], index: Index): (Shape, Queue[Sprite], Index) =
    val (k, shapesQueue2) = shapesQueue.dequeue
    (Shape(k, (2, height + 3)), shapesQueue2.enqueue(k), index.incSprite)

  def playN(display: Display2D[Char], height: Int, shapesQueue: Queue[Sprite], wind: Queue[Char], index: Index, count: Int): (Int, Index, Queue[Sprite], Queue[Char]) =
    if count == 0 then
      (height, index, shapesQueue, wind)
    else
      val (shape, shapesQueue2, index2) = spawn(height, shapesQueue, index)
      //printNshape(display, shape, 20)
      val (shapeRest, wind2, index3) = play1(display, shape, wind, index2)
      val newHeight = math.max(draw(display, shapeRest), height)
      playN(display, newHeight, shapesQueue2, wind2, index3, count - 1)
  // 2879
  lazy val answer1: Int =
    playN(display, 0, spritesQueue, wind, Index(0,0), 2022)._1

  //Part 2 // 1581706470999
  // 1567536231863 - too low
  // 1581449275319

  class State(var display: Display2D[Char], var height: Int, var index: Index, var shapes: Queue[Sprite], var wind: Queue[Char]):
    def step: Unit = 
      val (h, i, s, w) = playN(display, height, shapes, wind, index, 1)
      height = h
      index = i
      shapes = s
      wind = w

  lazy val answer2: Long =
    val n = 1_000_000_000_000L
    def initialState = State(display, 0, Index(0,0), spritesQueue, wind)
    def eq(s1: State, s2: State): Boolean = 
      s1.index == s2.index
    def copy(s1: State, s2: State): Unit = 
      s1.display = Display2D.fromIDisplay2D(s2.display.toIDisplay2D)
      s1.height = s2.height
      s1.index = s2.index
      s1.shapes = s2.shapes
      s1.wind = s2.wind

    val states = Array(initialState,initialState,initialState)
    val (shift1, period) = NumberSequenceUtils.floydMutable(states)(eq, copy)(_.step)
    val shift = shift1 + period // to eliminate initial disturbances

    val h1 = playN(display, 0, spritesQueue, wind, Index(0,0), shift.toInt)
    val h2 = playN(display, 0, spritesQueue, wind, Index(0,0), (shift+period).toInt)
    val Nrest = (n-shift) % period
    val hrest = playN(display, 0, spritesQueue, wind, Index(0,0), (shift + Nrest).toInt)
    val deltaHPerPeriod = h2._1 - h1._1
    val count = (n-shift)/period

    println(s"shift = $shift, period = $period, deltaHPerPeriod = $deltaHPerPeriod, hrest._1 = ${hrest._1}, h1._1 = ${h1._1}")
    deltaHPerPeriod * count + (hrest._1)

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
