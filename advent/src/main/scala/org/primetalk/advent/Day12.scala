package org.primetalk.advent

import org.primetalk.advent.SequenceUtils.unfoldN

/**
  * --- Day 12: Subterranean Sustainability ---
  *
  * The year 518 is significantly more underground than your history books implied. Either that, or you've arrived in a vast cavern network under the North Pole.
  *
  * After exploring a little, you discover a long tunnel that contains a row of small pots as far as you can see to your left and right. A few of them contain plants - someone is trying to grow things in these geothermally-heated caves.
  *
  * The pots are numbered, with 0 in front of you. To the left, the pots are numbered -1, -2, -3, and so on; to the right, 1, 2, 3.... Your puzzle input contains a list of pots from 0 to the right and whether they do (#) or do not (.) currently contain a plant, the initial state. (No other pots currently contain plants.) For example, an initial state of #..##.... indicates that pots 0, 3, and 4 currently contain plants.
  *
  * Your puzzle input also contains some notes you find on a nearby table: someone has been trying to figure out how these plants spread to nearby pots. Based on the notes, for each generation of plants, a given pot has or does not have a plant based on whether that pot (and the two pots on either side of it) had a plant in the last generation. These are written as LLCRR => N, where L are pots to the left, C is the current pot being considered, R are the pots to the right, and N is whether the current pot will have a plant in the next generation. For example:
  *
  * A note like ..#.. => . means that a pot that contains a plant but with no plants within two pots of it will not have a plant in it during the next generation.
  * A note like ##.## => . means that an empty pot with two plants on each side of it will remain empty in the next generation.
  * A note like .##.# => # means that a pot has a plant in a given generation if, in the previous generation, there were plants in that pot, the one immediately to the left, and the one two pots to the right, but not in the ones immediately to the right and two to the left.
  *
  * It's not clear what these plants are for, but you're sure it's important, so you'd like to make sure the current configuration of plants is sustainable by determining what will happen after 20 generations.
  *
  * For example, given the following input:
  *
  * initial state: #..#.#..##......###...###
  *
  * ...## => #
  * ..#.. => #
  * .#... => #
  * .#.#. => #
  * .#.## => #
  * .##.. => #
  * .#### => #
  * #.#.# => #
  * #.### => #
  * ##.#. => #
  * ##.## => #
  * ###.. => #
  * ###.# => #
  * ####. => #
  *
  * For brevity, in this example, only the combinations which do produce a plant are listed. (Your input includes all possible combinations.) Then, the next 20 generations will look like this:
  *
  *                  1         2         3
  *        0         0         0         0
  *  0: ...#..#.#..##......###...###...........
  *  1: ...#...#....#.....#..#..#..#...........
  *  2: ...##..##...##....#..#..#..##..........
  *  3: ..#.#...#..#.#....#..#..#...#..........
  *  4: ...#.#..#...#.#...#..#..##..##.........
  *  5: ....#...##...#.#..#..#...#...#.........
  *  6: ....##.#.#....#...#..##..##..##........
  *  7: ...#..###.#...##..#...#...#...#........
  *  8: ...#....##.#.#.#..##..##..##..##.......
  *  9: ...##..#..#####....#...#...#...#.......
  * 10: ..#.#..#...#.##....##..##..##..##......
  * 11: ...#...##...#.#...#.#...#...#...#......
  * 12: ...##.#.#....#.#...#.#..##..##..##.....
  * 13: ..#..###.#....#.#...#....#...#...#.....
  * 14: ..#....##.#....#.#..##...##..##..##....
  * 15: ..##..#..#.#....#....#..#.#...#...#....
  * 16: .#.#..#...#.#...##...#...#.#..##..##...
  * 17: ..#...##...#.#.#.#...##...#....#...#...
  * 18: ..##.#.#....#####.#.#.#...##...##..##..
  * 19: .#..###.#..#.#.#######.#.#.#..#.#...#..
  * 20: .#....##....#####...#######....#.#..##.
  *
  * The generation is shown along the left, where 0 is the initial state. The pot numbers are shown along the top, where 0 labels the center pot, negative-numbered pots extend to the left, and positive pots extend toward the right. Remember, the initial state begins at pot 0, which is not the leftmost pot used in this example.
  *
  * After one generation, only seven plants remain. The one in pot 0 matched the rule looking for ..#.., the one in pot 4 matched the rule looking for .#.#., pot 9 matched .##.., and so on.
  *
  * In this example, after 20 generations, the pots shown as # contain plants, the furthest left of which is pot -2, and the furthest right of which is pot 34. Adding up all the numbers of plant-containing pots after the 20th generation produces 325.
  *
  * After 20 generations, what is the sum of the numbers of all pots which contain a plant?
  *
  * Your puzzle answer was 1917.
  * --- Part Two ---
  *
  * You realize that 20 generations aren't enough. After all, these plants will need to last another 1500 years to even reach your timeline, not to mention your future.
  *
  * After fifty billion (50000000000) generations, what is the sum of the numbers of all pots which contain a plant?
  *
  * Your puzzle answer was 1250000000991.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day12 extends Utils {
  lazy val inputTextFromResource : Iterator[String] =
    readResource("day12.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq

  val initialState: String = lines.head.drop("initial state: ".length)

  case class Rule(pattern: String, result: Boolean)

  type Rules = Map[String, Boolean]

  type GenerativePatterns = Set[String]

  type ExtendedState = String // a number of dots added in the beginning and at the end

  def parseRule(line: String): Rule = {
    val Array(p, r) = line.split(" => ")
    Rule(p, r == "#")
  }

  //   val rulesMap = lines.drop(2).map(parseRule).map(r => (r.pattern, r.result)).toMap

  // Because there are exactly 32 combinations given, we only care about those that generate new plant.
  // All others will kill the plant.
  lazy val generativePatterns: GenerativePatterns =
    lines.drop(2)
      .map(parseRule)
      .filter(_.result).map(_.pattern)
      .toSet

  def generate(generativePatterns: GenerativePatterns)(state: String): String = {
    val chars: Seq[Char] = for{
      pos <- 2 until (state.length - 2)
      str = state.substring(pos - 2, pos + 3)
      isGenerated = generativePatterns.contains(str)
    } yield if(isGenerated) '#' else '.'
    ".." + new String(chars.toArray) + ".."
  }

  // 1917
  lazy val answer1: Int = {
    val prefixLength: Int = 22

    val prefix = "".padTo(prefixLength, '.')
    val state0: ExtendedState = prefix + initialState + prefix
    val N = 20
    val stateN = unfoldN(state0, N)(generate(generativePatterns))
    stateN.zipWithIndex.collect{ case (c, i) if c == '#' => i - prefixLength }.sum
  }

  def showGen(gen: Int, state: ExtendedState, prefixLength: Int): Unit =
    println(": " + gen + " " + state + " sum = " + state.zipWithIndex.collect{ case (c, i) if c == '#' => i - prefixLength }.sum)

  // Part 2
  val genIndex = 50000000000L

  // 1250000000991
  lazy val answer2: Long = {
    val prefixLength: Int = 122

    val prefix = "".padTo(prefixLength, '.')
    val state0: ExtendedState = prefix + initialState + prefix
    val N = 120 // this is enough to see the recurring pattern
    // In fact, all pots form a fixed pattern and travel to right in order.
    //
    val stateN = unfoldN(state0, N)(generate(generativePatterns))
    val sum = stateN.zipWithIndex.collect{ case (c, i) if c == '#' => i - prefixLength }.sum
    val cnt = stateN.count(_ == '#').toLong // all of them move to right.

    (genIndex - N) * cnt + sum
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
