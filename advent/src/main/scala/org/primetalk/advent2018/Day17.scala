package org.primetalk.advent2018

import org.primetalk.advent.tools.{Display, Utils}
import org.primetalk.advent.tools.Geom2dUtils._

/**
  * --- Day 17: Reservoir Research ---
  *
  * You arrive in the year 18. If it weren't for the coat you got in 1018, you would be very cold: the North Pole base hasn't even been constructed.
  *
  * Rather, it hasn't been constructed yet. The Elves are making a little progress, but there's not a lot of liquid water in this climate, so they're getting very dehydrated. Maybe there's more underground?
  *
  * You scan a two-dimensional vertical slice of the ground nearby and discover that it is mostly sand with veins of clay. The scan only provides data with a granularity of square meters, but it should be good enough to determine how much water is trapped there. In the scan, x represents the distance to the right, and y represents the distance down. There is also a spring of water near the surface at x=500, y=0. The scan identifies which square meters are clay (your puzzle input).
  *
  * For example, suppose your scan shows the following veins of clay:
  *
  * x=495, y=2..7
  * y=7, x=495..501
  * x=501, y=3..7
  * x=498, y=2..4
  * x=506, y=1..2
  * x=498, y=10..13
  * x=504, y=10..13
  * y=13, x=498..504
  *
  * Rendering clay as #, sand as ., and the water spring as +, and with x increasing to the right and y increasing downward, this becomes:
  *
  * 44444455555555
  * 99999900000000
  * 45678901234567
  * 0 ......+.......
  * 1 ............#.
  * 2 .#..#.......#.
  * 3 .#..#..#......
  * 4 .#..#..#......
  * 5 .#.....#......
  * 6 .#.....#......
  * 7 .#######......
  * 8 ..............
  * 9 ..............
  * 10 ....#.....#...
  * 11 ....#.....#...
  * 12 ....#.....#...
  * 13 ....#######...
  *
  * The spring of water will produce water forever. Water can move through sand, but is blocked by clay. Water always moves down when possible, and spreads to the left and right otherwise, filling space that has clay on both sides and falling out otherwise.
  *
  * For example, if five squares of water are created, they will flow downward until they reach the clay and settle there. Water that has come to rest is shown here as ~, while sand through which water has passed (but which is now dry again) is shown as |:
  *
  * ......+.......
  * ......|.....#.
  * .#..#.|.....#.
  * .#..#.|#......
  * .#..#.|#......
  * .#....|#......
  * .#~~~~~#......
  * .#######......
  * ..............
  * ..............
  * ....#.....#...
  * ....#.....#...
  * ....#.....#...
  * ....#######...
  *
  * Two squares of water can't occupy the same location. If another five squares of water are created, they will settle on the first five, filling the clay reservoir a little more:
  *
  * ......+.......
  * ......|.....#.
  * .#..#.|.....#.
  * .#..#.|#......
  * .#..#.|#......
  * .#~~~~~#......
  * .#~~~~~#......
  * .#######......
  * ..............
  * ..............
  * ....#.....#...
  * ....#.....#...
  * ....#.....#...
  * ....#######...
  *
  * Water pressure does not apply in this scenario. If another four squares of water are created, they will stay on the right side of the barrier, and no water will reach the left side:
  *
  * ......+.......
  * ......|.....#.
  * .#..#.|.....#.
  * .#..#~~#......
  * .#..#~~#......
  * .#~~~~~#......
  * .#~~~~~#......
  * .#######......
  * ..............
  * ..............
  * ....#.....#...
  * ....#.....#...
  * ....#.....#...
  * ....#######...
  *
  * At this point, the top reservoir overflows. While water can reach the tiles above the surface of the water, it cannot settle there, and so the next five squares of water settle like this:
  *
  * ......+.......
  * ......|.....#.
  * .#..#||||...#.
  * .#..#~~#|.....
  * .#..#~~#|.....
  * .#~~~~~#|.....
  * .#~~~~~#|.....
  * .#######|.....
  * ........|.....
  * ........|.....
  * ....#...|.#...
  * ....#...|.#...
  * ....#~~~~~#...
  * ....#######...
  *
  * Note especially the leftmost |: the new squares of water can reach this tile, but cannot stop there. Instead, eventually, they all fall to the right and settle in the reservoir below.
  *
  * After 10 more squares of water, the bottom reservoir is also full:
  *
  * ......+.......
  * ......|.....#.
  * .#..#||||...#.
  * .#..#~~#|.....
  * .#..#~~#|.....
  * .#~~~~~#|.....
  * .#~~~~~#|.....
  * .#######|.....
  * ........|.....
  * ........|.....
  * ....#~~~~~#...
  * ....#~~~~~#...
  * ....#~~~~~#...
  * ....#######...
  *
  * Finally, while there is nowhere left for the water to settle, it can reach a few more tiles before overflowing beyond the bottom of the scanned data:
  *
  * ......+.......    (line not counted: above minimum y value)
  * ......|.....#.
  * .#..#||||...#.
  * .#..#~~#|.....
  * .#..#~~#|.....
  * .#~~~~~#|.....
  * .#~~~~~#|.....
  * .#######|.....
  * ........|.....
  * ...|||||||||..
  * ...|#~~~~~#|..
  * ...|#~~~~~#|..
  * ...|#~~~~~#|..
  * ...|#######|..
  * ...|.......|..    (line not counted: below maximum y value)
  * ...|.......|..    (line not counted: below maximum y value)
  * ...|.......|..    (line not counted: below maximum y value)
  *
  * How many tiles can be reached by the water? To prevent counting forever, ignore tiles with a y coordinate smaller than the smallest y coordinate in your scan data or larger than the largest one. Any x coordinate is valid. In this example, the lowest y coordinate given is 1, and the highest is 13, causing the water spring (in row 0) and the water falling off the bottom of the render (in rows 14 through infinity) to be ignored.
  *
  * So, in the example above, counting both water at rest (~) and other sand tiles the water can hypothetically reach (|), the total number of tiles the water can reach is 57.
  *
  * How many tiles can the water reach within the range of y values in your scan?
  *
  * Your puzzle answer was 37649.
  * --- Part Two ---
  *
  * After a very long time, the water spring will run dry. How much water will be retained?
  *
  * In the example above, water that won't eventually drain out is shown as ~, a total of 29 tiles.
  *
  * How many water tiles are left after the water spring stops producing water and all remaining water not at rest has drained?
  *
  * Your puzzle answer was 30112.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day17 extends Utils {

  lazy val inputTextFromResource : Iterator[String] =
    readResource("day17.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq

  val sand = '.'

  val clay = '#'

  val waterFall = '|'

  val ice = '~'

  val springOfWaterPosition: Position = (500, 0)

  sealed trait LineOfClay {
    def boundingRect: Rectangle
  }

  case class VerticalLineOfClay(x: Int, minY: Int, maxY: Int) extends LineOfClay {
    def boundingRect: Rectangle = rectangleByDiagonal((x, minY), (x, maxY))
  }

  case class HorizontalLineOfClay(y: Int, minX: Int, maxX: Int) extends LineOfClay {
    def boundingRect: Rectangle = rectangleByDiagonal((minX, y), (maxX, y))
  }

  def parseLine(line: String): LineOfClay = {
    val Seq(a, b, c) = parseAllIntsInString(line)
    line.charAt(0) match {
      case 'x' => VerticalLineOfClay(a, b, c)
      case 'y' => HorizontalLineOfClay(a, b, c)
    }
  }

  def linesOfClay: Seq[LineOfClay] = lines.map(parseLine)

  def drawLines(linesOfClay: Seq[LineOfClay]): Display[Char] = {
    val rect = boundingRect(springOfWaterPosition +: linesOfClay.flatMap(_.boundingRect.coordinatePoints))
    // we enlarge size by 2 columns - one to left and another one to right
    val d = new Display[Char](rect.topLeft + (-1, 0), rect.size + (2,0))()
    d.fillAll(sand)
    linesOfClay.foreach{
      case VerticalLineOfClay(x, minY, maxY) =>
        (minY to maxY).map(y => (x, y)).foreach(p => d(p) = clay)
      case HorizontalLineOfClay(y, minX, maxX) =>
        (minX to maxX).map(x => (x, y)).foreach(p => d(p) = clay)
    }
    d(springOfWaterPosition) = '+'
    d
  }

  def showDisplay(d: Display[Char]): Unit = {
    for{
      y <- d.ys
    } {
      println(new String(d.lineY(y)))
    }
  }

  /** Flows water from spring.
    * p - is the next position that has water above it.
    */
  def flowVertical(d: Display[Char], step: Vector2d)(p: Position): Position = {
    if(d.isWithinRange(p) && d(p) == sand) {
      d(p) = waterFall
      flowVertical(d, step)(p + step)
    } else p
  }

  def flowHorizontal(d: Display[Char], step: Vector2d)(p: Position): Position = {
    if(d(p) == clay)
      p
    else {
      d(p) = waterFall
      if (d.isWithinRange(p) && d(p + (0, 1)) == clay || d(p + (0, 1)) == ice) {
        flowHorizontal(d, step)(p + step)
      } else p
    }
  }

  def drawIce(d: Display[Char], from: (Int, Int), to: (Int, Int)): Unit = {
    for{
      x <- from._1 to to._1
    } d( (x, from._2)) = ice
  }

  def runWaterFall(d: Display[Char], springs: List[Position]): Unit = springs match {
    case Nil => ()
    case spring :: tail =>
//      if(d((d.maxX, d.maxY)) == sand) print(1)
      val nextPos: Position = flowVertical(d, (0,1))(spring + (0, 1))
      val newSprings =
        if(d.isWithinRange(nextPos)) {
          if (d(nextPos) == clay || d(nextPos) == ice) {
            val upperPoint = nextPos + (0, -1)
//            d(upperPoint) = waterFall
            val leftStop = flowHorizontal(d, (-1, 0))(upperPoint)
            val rightStop = flowHorizontal(d, (1, 0))(upperPoint + (+1, 0))
            if (d(leftStop) == clay && d(rightStop) == clay) {
              drawIce(d, leftStop + Right, rightStop + Left)
              List(nextPos + (0, -2))
            } else
              List(leftStop, rightStop)
                .filter(p => d(p) == waterFall)
          } else List()
        } else List()
      runWaterFall(d, newSprings reverse_::: tail)
  }

  def countWater(d: Display[Char], predicate: Char => Boolean): Int = {
    (for{
      p <- d.points

      if predicate(d(p))
    } yield 1).size
  }
  // 37654
  // -5 = 37649 (correction for the lines above highest clay)
  lazy val answer1: Long = {
    val display = drawLines(linesOfClay)
    runWaterFall(display, List(springOfWaterPosition))
    showDisplay(display)
    countWater(display, c => c == ice || c == waterFall) - 5
  }

  // Part 2
  // 30112
  lazy val answer2: Long = {
    val display = drawLines(linesOfClay)
    runWaterFall(display, List(springOfWaterPosition))
    showDisplay(display)
    countWater(display, c => c == ice)
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
