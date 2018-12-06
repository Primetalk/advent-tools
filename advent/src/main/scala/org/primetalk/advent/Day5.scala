package org.primetalk.advent

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
object Day5 extends Utils {
  lazy val inputTextFromResource : Iterator[String] =
    readResource("day5.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq.sorted

  lazy val line: String = lines.head

  def charMatch(c1: Char, c2: Char): Boolean =
    (c1 != c2) && c1.toLower == c2.toLower

  def react(arr: Array[Char]): Int = {
    val len = arr.length
    var clearedCount = 0
    def clear(i: Int, j: Int): Unit = {
      arr(i) = '0'
      arr(j) = '0'
      clearedCount += 2
    }


    def go(i: Int, j: Int): Int = {
      if(i < 0) {
        go(j, j + 1)
      } else if(i == len - 1 || j > len - 1)
        len - clearedCount
      else if(arr(i) == '0')
        go(i - 1, j)
      else {
        if(charMatch(arr(i), arr(j))) {
          clear(i, j)
          go(i - 1, j + 1) //
        } else
          go(j, j + 1)

      }
    }
    go(0, 1)
  }






  def answer1: Int = 1 // react(line.toCharArray)

  // Part 2



  def answer2: Int = {
    val lengths = for{
      a <- 'a' until 'z'
      reducedLine = line.filter(_.toLower != a)
      i = react(reducedLine.toCharArray)
    } yield i
    lengths.min
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }
}
