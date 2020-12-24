package org.primetalk.advent2018

import org.primetalk.advent.tools.SequenceUtils.{floydInt, unfoldN}
import org.primetalk.advent.tools.{Display, Utils}

/**
  * --- Day 18: Settlers of The North Pole ---
  *
  * On the outskirts of the North Pole base construction project, many Elves are collecting lumber.
  *
  * The lumber collection area is 50 acres by 50 acres; each acre can be either open ground (.), trees (|), or a lumberyard (#). You take a scan of the area (your puzzle input).
  *
  * Strange magic is at work here: each minute, the landscape looks entirely different. In exactly one minute, an open acre can fill with trees, a wooded acre can be converted to a lumberyard, or a lumberyard can be cleared to open ground (the lumber having been sent to other projects).
  *
  * The change to each acre is based entirely on the contents of that acre as well as the number of open, wooded, or lumberyard acres adjacent to it at the start of each minute. Here, "adjacent" means any of the eight acres surrounding that acre. (Acres on the edges of the lumber collection area might have fewer than eight adjacent acres; the missing acres aren't counted.)
  *
  * In particular:
  *
  * An open acre will become filled with trees if three or more adjacent acres contained trees. Otherwise, nothing happens.
  * An acre filled with trees will become a lumberyard if three or more adjacent acres were lumberyards. Otherwise, nothing happens.
  * An acre containing a lumberyard will remain a lumberyard if it was adjacent to at least one other lumberyard and at least one acre containing trees. Otherwise, it becomes open.
  *
  * These changes happen across all acres simultaneously, each of them using the state of all acres at the beginning of the minute and changing to their new form by the end of that same minute. Changes that happen during the minute don't affect each other.
  *
  * For example, suppose the lumber collection area is instead only 10 by 10 acres with this initial configuration:
  *
  * Initial state:
  * .#.#...|#.
  * .....#|##|
  * .|..|...#.
  * ..|#.....#
  * #.#|||#|#|
  * ...#.||...
  * .|....|...
  * ||...#|.#|
  * |.||||..|.
  * ...#.|..|.
  *
  * After 1 minute:
  * .......##.
  * ......|###
  * .|..|...#.
  * ..|#||...#
  * ..##||.|#|
  * ...#||||..
  * ||...|||..
  * |||||.||.|
  * ||||||||||
  * ....||..|.
  *
  * After 2 minutes:
  * .......#..
  * ......|#..
  * .|.|||....
  * ..##|||..#
  * ..###|||#|
  * ...#|||||.
  * |||||||||.
  * ||||||||||
  * ||||||||||
  * .|||||||||
  *
  * After 3 minutes:
  * .......#..
  * ....|||#..
  * .|.||||...
  * ..###|||.#
  * ...##|||#|
  * .||##|||||
  * ||||||||||
  * ||||||||||
  * ||||||||||
  * ||||||||||
  *
  * After 4 minutes:
  * .....|.#..
  * ...||||#..
  * .|.#||||..
  * ..###||||#
  * ...###||#|
  * |||##|||||
  * ||||||||||
  * ||||||||||
  * ||||||||||
  * ||||||||||
  *
  * After 5 minutes:
  * ....|||#..
  * ...||||#..
  * .|.##||||.
  * ..####|||#
  * .|.###||#|
  * |||###||||
  * ||||||||||
  * ||||||||||
  * ||||||||||
  * ||||||||||
  *
  * After 6 minutes:
  * ...||||#..
  * ...||||#..
  * .|.###|||.
  * ..#.##|||#
  * |||#.##|#|
  * |||###||||
  * ||||#|||||
  * ||||||||||
  * ||||||||||
  * ||||||||||
  *
  * After 7 minutes:
  * ...||||#..
  * ..||#|##..
  * .|.####||.
  * ||#..##||#
  * ||##.##|#|
  * |||####|||
  * |||###||||
  * ||||||||||
  * ||||||||||
  * ||||||||||
  *
  * After 8 minutes:
  * ..||||##..
  * ..|#####..
  * |||#####|.
  * ||#...##|#
  * ||##..###|
  * ||##.###||
  * |||####|||
  * ||||#|||||
  * ||||||||||
  * ||||||||||
  *
  * After 9 minutes:
  * ..||###...
  * .||#####..
  * ||##...##.
  * ||#....###
  * |##....##|
  * ||##..###|
  * ||######||
  * |||###||||
  * ||||||||||
  * ||||||||||
  *
  * After 10 minutes:
  * .||##.....
  * ||###.....
  * ||##......
  * |##.....##
  * |##.....##
  * |##....##|
  * ||##.####|
  * ||#####|||
  * ||||#|||||
  * ||||||||||
  *
  * After 10 minutes, there are 37 wooded acres and 31 lumberyards. Multiplying the number of wooded acres by the number of lumberyards gives the total resource value after ten minutes: 37 * 31 = 1147.
  *
  * What will the total resource value of the lumber collection area be after 10 minutes?
  *
  * Your puzzle answer was 511000.
  * --- Part Two ---
  *
  * This important natural resource will need to last for at least thousands of years. Are the Elves collecting this lumber sustainably?
  *
  * What will the total resource value of the lumber collection area be after 1000000000 minutes?
  *
  * Your puzzle answer was 194934.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day18 extends Utils {

  lazy val inputTextFromResource : Iterator[String] =
    readResource("day18.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq

  type CellState = Char

  val OpenGround: CellState = '.'

  val Tree: CellState = '|'

  val Lumberyard: CellState = '#'

  def rules(currentState: CellState, valuesAround: Seq[CellState]): CellState = {
    def trees = valuesAround.count(_ == Tree)
    def lumberyards = valuesAround.count(_ == Lumberyard)
    currentState match {
      case OpenGround =>
        if(trees >= 3) Tree else OpenGround
      case Tree =>
        if(lumberyards >= 3) Lumberyard else Tree
      case Lumberyard =>
        if(trees >=1 && lumberyards >= 1) Lumberyard else OpenGround
    }
  }

  def resourceValue(d: Display[Char]): Int = {
    d.values.count(_ == Tree) *
      d.values.count(_ == Lumberyard)
  }


  /*
What will the total resource value of the lumber collection area be after 10 minutes?
   */
  lazy val answer1: Long = {
    val initialState = Display.readCharDisplay(lines)
    val N = 10
    val stateN = unfoldN(initialState, N)(_.produceByLocalRules8(rules))
    resourceValue(stateN)
  }

  // Part 2

  val time = 1000000000

  // 191556
  // Period = 28
  // 194934
  lazy val answer2: Long = {
    val initialState = Display.readCharDisplay(lines)
    val (start, loopLength) = floydInt(initialState)(Display.eq)(_.produceByLocalRules8(rules))
    val phase = (time - start) % loopLength
    val N = start + phase // This is the first position when state is identical to the state at $time
    val stateN = unfoldN(initialState, N)(_.produceByLocalRules8(rules))
    resourceValue(stateN)
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
