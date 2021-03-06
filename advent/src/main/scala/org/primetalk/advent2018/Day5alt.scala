package org.primetalk.advent2018

import org.primetalk.advent.tools.Utils

/**
  * --- Day 5: Alchemical Reduction ---
  *
  * You've managed to sneak in to the prototype suit manufacturing lab. The Elves are making decent progress, but are still struggling with the suit's size reduction capabilities.
  *
  * While the very latest in 1518 alchemical technology might have solved their problem eventually, you can do better. You scan the chemical composition of the suit's material and discover that it is formed by extremely long polymers (one of which is available as your puzzle input).
  *
  * The polymer is formed by smaller units which, when triggered, react with each other such that two adjacent units of the same type and opposite polarity are destroyed. Units' types are represented by letters; units' polarity is represented by capitalization. For instance, r and R are units with the same type but opposite polarity, whereas r and s are entirely different types and do not react.
  *
  * For example:
  *
  * In aA, a and A react, leaving nothing behind.
  * In abBA, bB destroys itself, leaving aA. As above, this then destroys itself, leaving nothing.
  * In abAB, no two adjacent units are of the same type, and so nothing happens.
  * In aabAAB, even though aa and AA are of the same type, their polarities match, and so nothing happens.
  *
  * Now, consider a larger example, dabAcCaCBAcCcaDA:
  *
  * dabAcCaCBAcCcaDA  The first 'cC' is removed.
  * dabAaCBAcCcaDA    This creates 'Aa', which is removed.
  * dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
  * dabCBAcaDA        No further actions can be taken.
  *
  * After all possible reactions, the resulting polymer contains 10 units.
  *
  * How many units remain after fully reacting the polymer you scanned? (Note: in this puzzle and others, the input is large; if you copy/paste your input, make sure you get the whole thing.)
  *
  * Your puzzle answer was 10972.
  * --- Part Two ---
  *
  * Time to improve the polymer.
  *
  * One of the unit types is causing problems; it's preventing the polymer from collapsing as much as it should. Your goal is to figure out which unit type is causing the most problems, remove all instances of it (regardless of polarity), fully react the remaining polymer, and measure its length.
  *
  * For example, again using the polymer dabAcCaCBAcCcaDA from above:
  *
  * Removing all A/a units produces dbcCCBcCcD. Fully reacting this polymer produces dbCBcD, which has length 6.
  * Removing all B/b units produces daAcCaCAcCcaDA. Fully reacting this polymer produces daCAcaDA, which has length 8.
  * Removing all C/c units produces dabAaBAaDA. Fully reacting this polymer produces daDA, which has length 4.
  * Removing all D/d units produces abAcCaCBAcCcaA. Fully reacting this polymer produces abCBAc, which has length 6.
  *
  * In this example, removing all C/c units was best, producing the answer 4.
  *
  * What is the length of the shortest polymer you can produce by removing all units of exactly one type and fully reacting the result?
  *
  * Your puzzle answer was 5278.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day5alt extends Utils {
  lazy val inputTextFromResource : Iterator[String] =
    readResource("day5.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq.sorted

  lazy val line: String = lines.head

  def charMatch(c1: Char, c2: Char): Boolean =
    (c1 != c2) && c1.toLower == c2.toLower

  def react(left: List[Char] = List(), vec: Vector[Char]): Int = {
    if(vec.isEmpty)
      left.size
    else {
      left match {
        case Nil => react(vec.head :: Nil, vec.tail)
        case h :: t =>
          val vh = vec.head
          val vt = vec.tail
          if(charMatch(h, vh))
            react(t, vt)
          else
            react(vh :: left, vt)
      }
    }
  }

  def answer1: Int = react(List(), line.toCharArray.toVector)

  // Part 2
  def answer2: Int = {
    val lengths = for{
      a <- 'a' until 'z'
      reducedLine = line.filter(_.toLower != a)
      i = react(List(), reducedLine.toCharArray.toVector)
    } yield i
    lengths.min
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }
}
