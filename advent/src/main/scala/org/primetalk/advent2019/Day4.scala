package org.primetalk.advent2019

/**
  * https://adventofcode.com/2019/day/4
  *
  * --- Day 4: Secure Container ---
  *
  * You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the password on a sticky note, but someone threw it out.
  *
  * However, they do remember a few key facts about the password:
  *
  * It is a six-digit number.
  * The value is within the range given in your puzzle input.
  * Two adjacent digits are the same (like 22 in 122345).
  * Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
  *
  * Other than the range rule, the following are true:
  *
  * 111111 meets these criteria (double 11, never decreases).
  * 223450 does not meet these criteria (decreasing pair of digits 50).
  * 123789 does not meet these criteria (no double).
  *
  * How many different passwords within the range given in your puzzle input meet these criteria?
  *
  * Your puzzle answer was 481.
  * --- Part Two ---
  *
  * An Elf just remembered one more important detail: the two adjacent matching digits are not part of a larger group of matching digits.
  *
  * Given this additional criterion, but still ignoring the range rule, the following are now true:
  *
  * 112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long.
  * 123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
  * 111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22).
  *
  * How many different passwords within the range given in your puzzle input meet all of the criteria?
  *
  * Your puzzle answer was 299.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  *
  */
object Day4 {

  val range: Range.Inclusive = 372037 to 905157

  def is6digitNumber(p: Int): Boolean = p >= 100000 && p < 1000000

  @scala.annotation.tailrec
  def digits10(p: Int, digits: List[Byte] = Nil): List[Byte] =
    if(p == 0) digits
    else digits10(p/10, (p%10).toByte :: digits)

  @scala.annotation.tailrec
  def increaseOrSame(d: List[Byte], hasDuplicates: Boolean = false): Boolean = d match {
    case h1 :: h2 :: t => h2 >= h1 && increaseOrSame(h2::t, hasDuplicates || h1 == h2)
    case _ => hasDuplicates
  }

  //is6digitNumber(p) && isWithinTheRange(p) - know in advance
  def isAGoodPassword(p: Int): Boolean = {
    val digits = digits10(p)
    increaseOrSame(digits)
  }

  lazy val answer1: Long = {
    range.count(isAGoodPassword)
  }

  def hasPair(d: List[Byte]): Boolean = {
    val sizes = d.groupBy(identity).map(_._2.size)
    sizes.exists(_ == 2)
  }

  def isAGoodPassword2(p: Int): Boolean = {
    val digits = digits10(p)
    increaseOrSame(digits) && hasPair(digits)
  }

  lazy val answer2: Long = range.count(isAGoodPassword2)

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
