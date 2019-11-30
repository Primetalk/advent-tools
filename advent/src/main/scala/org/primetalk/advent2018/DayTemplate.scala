package org.primetalk.advent2018

import org.primetalk.advent.tools.Utils

object DayTemplate extends Utils {

  lazy val inputTextFromResource : Iterator[String] =
    readResource("dayInput.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq

  lazy val answer1: Long = 1

  // Part 2

  lazy val answer2: Long = 2

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
