package org.primetalk.advent2018

import org.primetalk.advent.tools.Utils

import scala.util.matching.Regex

/**
  * --- Day 3: No Matter How You Slice It ---
  *
  * The Elves managed to locate the chimney-squeeze prototype fabric for Santa's suit (thanks to someone who helpfully wrote its box IDs on the wall of the warehouse in the middle of the night). Unfortunately, anomalies are still affecting them - nobody can even agree on how to cut the fabric.
  *
  * The whole piece of fabric they're working on is a very large square - at least 1000 inches on each side.
  *
  * Each Elf has made a claim about which area of fabric would be ideal for Santa's suit. All claims have an ID and consist of a single rectangle with edges parallel to the edges of the fabric. Each claim's rectangle is defined as follows:
  *
  * The number of inches between the left edge of the fabric and the left edge of the rectangle.
  * The number of inches between the top edge of the fabric and the top edge of the rectangle.
  * The width of the rectangle in inches.
  * The height of the rectangle in inches.
  *
  * A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall. Visually, it claims the square inches of fabric represented by # (and ignores the square inches of fabric represented by .) in the diagram below:
  *
  * ...........
  * ...........
  * ...#####...
  * ...#####...
  * ...#####...
  * ...#####...
  * ...........
  * ...........
  * ...........
  *
  * The problem is that many of the claims overlap, causing two or more claims to cover part of the same areas. For example, consider the following claims:
  *
  * #1 @ 1,3: 4x4
  * #2 @ 3,1: 4x4
  * #3 @ 5,5: 2x2
  *
  * Visually, these claim the following areas:
  *
  * ........
  * ...2222.
  * ...2222.
  * .11XX22.
  * .11XX22.
  * .111133.
  * .111133.
  * ........
  *
  * The four square inches marked with X are claimed by both 1 and 2. (Claim 3, while adjacent to the others, does not overlap either of them.)
  *
  * If the Elves all proceed with their own plans, none of them will have enough fabric. How many square inches of fabric are within two or more claims?
  *
  * The first half of this puzzle is complete! It provides one gold star: *
  * --- Part Two ---
  *
  * Amidst the chaos, you notice that exactly one claim doesn't overlap by even a single square inch of fabric with any other claim. If you can somehow draw attention to it, maybe the Elves will be able to make Santa's suit after all!
  *
  * For example, in the claims above, only claim 3 is intact after all claims are made.
  *
  * What is the ID of the only claim that doesn't overlap?
  */
object Day3 extends Utils {
  lazy val inputTextFromResource : Iterator[String] =
    readResource("day3.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq

  case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int)

  val regex: Regex = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r("id", "left", "top", "width", "height")

  def parse(str: String): Claim = {
    val m = regex.findFirstMatchIn(str)
      .getOrElse(throw new IllegalArgumentException("Couldn't parse " + str))
    Claim(
      id = m.group("id").toInt,
      left = m.group("left").toInt,
      top = m.group("top").toInt,
      width = m.group("width").toInt,
      height = m.group("height").toInt,
    )
  }

  lazy val claims: Seq[Claim] = lines.map(parse)

  val maxSize = 1000
  type Display = Array[Array[Int]]

  def newDisplay(width: Int, height: Int): Display =
    Array.ofDim(width, height)

  def draw(display: Display)(claim: Claim): Unit = claim match {
    case Claim(id, left, top, width, height) =>
      def go(x: Int, y: Int): Unit = {
        if(y == height)
          ()
        else {
          if (x == width) {
            go(0, y + 1)
          } else {
            display(left + x)(top + y) =
              if (display(left + x)(top + y) != 0)
                -1
              else
                id
            go(x + 1, y)
          }
        }
      }
      go(0, 0)
  }

  def drawAll(display: Display)(claims: Seq[Claim]): Unit =
    claims.foreach(draw(display))

  def countIntersections(display: Display): Int = {
    def go(x: Int, y: Int, accum: Int): Int = {
      if (y == maxSize)
        accum
      else {
        if (x == maxSize) {
          go(0, y + 1, accum)
        } else {
          go(x + 1, y, accum + (if (display(x)(y) == -1) 1 else 0))
        }
      }
    }

    go(0, 0, 0)
  }

  lazy val answer1: Int = {
    val display: Array[Array[Int]] = newDisplay(maxSize, maxSize)
    drawAll(display)(claims)
    countIntersections(display)
  }

  def isItIntact(display: Display)(claim: Claim): Boolean = claim match {
    case Claim(id, left, top, width, height) =>
      def go(x: Int, y: Int, intact: Boolean): Boolean = {
        if(!intact || y == height)
          intact
        else {
          if (x == width) {
            go(0, y + 1, intact)
          } else {
            go(x + 1, y, display(left + x)(top + y) == id)
          }
        }
      }
      go(0, 0, intact = true)
  }

  def filterIntact(claims: Seq[Claim]): Seq[Claim] = {
    val display: Array[Array[Int]] = newDisplay(maxSize, maxSize)
    drawAll(display)(claims)
    claims.filter(isItIntact(display))
  }

  lazy val answer2: Seq[Claim] = filterIntact(claims)

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }
}
