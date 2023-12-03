package org.primetalk.advent2023

import org.primetalk.advent3.tools.{IDisplay2D, Utils}
import org.primetalk.advent3.tools.Geom2dUtils.*

import scala.annotation.tailrec
/**
  * https://adventofcode.com/2023/day/2
--- Day 3: Gear Ratios ---

You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.

It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.

"Aaah!"

You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.

The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.

The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)

Here is an example engine schematic:

467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?

Your puzzle answer was 535235.
--- Part Two ---

The engineer finds the missing part and installs it in the engine! As the engine springs to life, you jump in the closest gondola, finally ready to ascend to the water source.

You don't seem to be going very fast, though. Maybe something is still wrong? Fortunately, the gondola has a phone labeled "help", so you pick it up and the engineer answers.

Before you can explain the situation, she suggests that you look out the window. There stands the engineer, holding a phone in one hand and waving with the other. You're going so slowly that you haven't even left the station. You exit the gondola.

The missing part wasn't the only issue - one of the gears in the engine is wrong. A gear is any * symbol that is adjacent to exactly two part numbers. Its gear ratio is the result of multiplying those two numbers together.

This time, you need to find the gear ratio of every gear and add them all up so that the engineer can figure out which gear needs to be replaced.

Consider the same engine schematic again:

467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

In this schematic, there are two gears. The first is in the top left; it has part numbers 467 and 35, so its gear ratio is 16345. The second gear is in the lower right; its gear ratio is 451490. (The * adjacent to 617 is not a gear because it is only adjacent to one part number.) Adding up all of the gear ratios produces 467835.

What is the sum of all of the gear ratios in your engine schematic?

Your puzzle answer was 79844424.

Both parts of this puzzle are complete! They provide two gold stars: **
 */
object Day2303 extends Utils:

  val input: String = readThisObjectInput

  val display: IDisplay2D[Char] = IDisplay2D.readCharDisplayFromString(input, '.')
//  val lines: Seq[String] = readThisObjectInputLines

  inline def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
  inline def isEmpty(c: Char): Boolean = c == '.'
  inline def isGear(c: Char): Boolean = c == '*'

  def findNumberStartPositions(display: IDisplay2D[Char]): Seq[Position] =
    display.points.filter{
      pos =>
        val l = pos + Left
        isDigit(display(pos)) &&
          (
            !display.isWithinRange(l) ||
              !isDigit(display(l))
          )
    }

  @tailrec
  def thereAreSymbolsAroundNumberStartingAt(display: IDisplay2D[Char])(p: Position): Boolean =
    display.valuesAround(p)
      .exists(ch => !isDigit(ch) && !isEmpty(ch)) || {
      val r = p + Right
      display.isWithinRange(r) &&  isDigit(display(r)) && thereAreSymbolsAroundNumberStartingAt(display)(r)
    }

  def readNumber(display: IDisplay2D[Char])(p: Position, chars: List[Char] = Nil): Int =
    lazy val ch = display(p)
    if display.isWithinRange(p) && isDigit(ch) then
      readNumber(display)(p+Right, ch :: chars)
    else
      chars.reverse.mkString("").toInt

  // 590285 582531
  lazy val answer1: Int =
    findNumberStartPositions(display)
      .filter(thereAreSymbolsAroundNumberStartingAt(display))
      .map(readNumber(display)(_)).sum

  //Part 2
  lazy val answer2: Int =
    val gearPositions = display.points.filter(p => isGear(display(p)))
    val numberPositions = findNumberStartPositions(display)
    case class Num(pos: Position, n: Int):
      val length = n.toString.length
      val positions = (0 until length).map(i => (pos._1 + i, pos._2))

    val numberPositionsValuesAndLengths = numberPositions.map{
      p =>
        val n = readNumber(display)(p)
        Num(p, n)
      }
    gearPositions
      .map{ gearPos =>
        numberPositionsValuesAndLengths.filter(n => n.positions.exists(np => directions8.exists(dir => np + dir == gearPos)))
      }
      .filter(_.size == 2)
      .map{ case Seq(n1, n2) => n1.n * n2.n}
      .sum

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)

end Day2303
