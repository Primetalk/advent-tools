package org.primetalk.advent2023

import org.primetalk.advent3.tools.{IDisplay2D, Utils}

/**
  * https://adventofcode.com/2023/day/14
  */
object Day2314 extends Utils:

  val input: String = readThisObjectInput

  val display = IDisplay2D.readCharDisplayFromString(input, '.')
  
  val displayT = display.transpose
  
  def total(lineReversed: List[Char]): Int =
    lineReversed.zipWithIndex.filter(_._1 == 'O').map(_._2 + 1).sum
  def moveRoundRocksToLeftReversed(line: List[Char], res: List[Char] = Nil, dots: List[Char] = Nil): List[Char] =
    line match
      case Nil => (dots ::: res)
      case head :: tail =>
        head match
          case 'O' => moveRoundRocksToLeftReversed(tail, head :: res, dots)
          case '.' => moveRoundRocksToLeftReversed(tail, res, head :: dots)
          case '#' => moveRoundRocksToLeftReversed(tail, head :: dots ::: res, Nil)
  lazy val answer1: Int =
    val lines = displayT.ys.map(y => displayT.lineY(y).toList)
    val tilted = lines.map(line => moveRoundRocksToLeftReversed(line))
    tilted.map(total).sum

  //Part 2
  lazy val answer2: Int = 2

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
