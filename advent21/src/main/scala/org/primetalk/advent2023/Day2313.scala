package org.primetalk.advent2023

import org.primetalk.advent3.tools.{Bits, IDisplay2D, Utils}

import scala.annotation.tailrec

/**
  * https://adventofcode.com/2023/day/13
--- Day 13: Point of Incidence ---

With your help, the hot springs team locates an appropriate spring which launches you neatly and precisely up to the edge of Lava Island.

There's just one problem: you don't see any lava.

You do see a lot of ash and igneous rock; there are even what look like gray mountains scattered around. After a while, you make your way to a nearby cluster of mountains only to discover that the valley between them is completely full of large mirrors. Most of the mirrors seem to be aligned in a consistent way; perhaps you should head in that direction?

As you move through the valley of mirrors, you find that several of them have fallen from the large metal frames keeping them in place. The mirrors are extremely flat and shiny, and many of the fallen mirrors have lodged into the ash at strange angles. Because the terrain is all one color, it's hard to tell where it's safe to walk or where you're about to run into a mirror.

You note down the patterns of ash (.) and rocks (#) that you see as you walk (your puzzle input); perhaps by carefully analyzing these patterns, you can figure out where the mirrors are!

For example:

#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#

To find the reflection in each pattern, you need to find a perfect reflection across either a horizontal line between two rows or across a vertical line between two columns.

In the first pattern, the reflection is across a vertical line between two columns; arrows on each of the two columns point at the line between the columns:

123456789
    ><
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.
    ><
123456789

In this pattern, the line of reflection is the vertical line between columns 5 and 6. Because the vertical line is not perfectly in the middle of the pattern, part of the pattern (column 1) has nowhere to reflect onto and can be ignored; every other column has a reflected column within the pattern and must match exactly: column 2 matches column 9, column 3 matches 8, 4 matches 7, and 5 matches 6.

The second pattern reflects across a horizontal line instead:

1 #...##..# 1
2 #....#..# 2
3 ..##..### 3
4v#####.##.v4
5^#####.##.^5
6 ..##..### 6
7 #....#..# 7

This pattern reflects across the horizontal line between rows 4 and 5. Row 1 would reflect with a hypothetical row 8, but since that's not in the pattern, row 1 doesn't need to match anything. The remaining rows match: row 2 matches row 7, row 3 matches row 6, and row 4 matches row 5.

To summarize your pattern notes, add up the number of columns to the left of each vertical line of reflection; to that, also add 100 multiplied by the number of rows above each horizontal line of reflection. In the above example, the first pattern's vertical line has 5 columns to its left and the second pattern's horizontal line has 4 rows above it, a total of 405.

Find the line of reflection in each of the patterns in your notes. What number do you get after summarizing all of your notes?

Your puzzle answer was 35538.
--- Part Two ---

You resume walking through the valley of mirrors and - SMACK! - run directly into one. Hopefully nobody was watching, because that must have been pretty embarrassing.

Upon closer inspection, you discover that every mirror has exactly one smudge: exactly one . or # should be the opposite type.

In each pattern, you'll need to locate and fix the smudge that causes a different reflection line to be valid. (The old reflection line won't necessarily continue being valid after the smudge is fixed.)

Here's the above example again:

#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#

The first pattern's smudge is in the top-left corner. If the top-left # were instead ., it would have a different, horizontal line of reflection:

1 ..##..##. 1
2 ..#.##.#. 2
3v##......#v3
4^##......#^4
5 ..#.##.#. 5
6 ..##..##. 6
7 #.#.##.#. 7

With the smudge in the top-left corner repaired, a new horizontal line of reflection between rows 3 and 4 now exists. Row 7 has no corresponding reflected row and can be ignored, but every other row matches exactly: row 1 matches row 6, row 2 matches row 5, and row 3 matches row 4.

In the second pattern, the smudge can be fixed by changing the fifth symbol on row 2 from . to #:

1v#...##..#v1
2^#...##..#^2
3 ..##..### 3
4 #####.##. 4
5 #####.##. 5
6 ..##..### 6
7 #....#..# 7

Now, the pattern has a different horizontal line of reflection between rows 1 and 2.

Summarize your notes as before, but instead use the new different reflection lines. In this example, the first pattern's new horizontal line has 3 rows above it and the second pattern's new horizontal line has 1 row above it, summarizing to the value 400.

In each pattern, fix the smudge and find the different line of reflection. What number do you get after summarizing the new reflection line in each pattern in your notes?

Your puzzle answer was 30442.

Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2313 extends Utils:

  val input: String = readThisObjectInput

  type Pattern = IDisplay2D[Char]
  val patterns: Seq[Pattern] = input
    .split("\n\n").toList
    .map(p => IDisplay2D.readCharDisplayFromString(p, '.'))
    .map(_
      .replace('.', '0')
      .replace('#', '1'))

  @tailrec
  def reflectivePos(list: List[Long], rev: List[Long], pos: Int, len: Int): Option[Int] =
    if pos == 0 then
      reflectivePos(list.tail, list.head::rev, 1, len)
    else if pos == len then
      None
    else
      if (pos <= len / 2 && list.startsWith(rev)) || (pos > len / 2 && rev.startsWith(list)) then
        Some(pos)
      else
        reflectivePos(list.tail, list.head::rev, pos + 1, len)
  def reflectiveRow(bin: Pattern): Option[Int] =
    val longs = bin.rows.map(a => BigInt.apply(a.mkString(""), 2).toLong)
    reflectivePos(longs.toList, Nil, 0, longs.size)

  def reflectiveRowOrColumn(p: Pattern): Int =
    reflectiveRow(p)
      .map(_ * 100)
      .orElse(reflectiveRow(p.transpose))
      .getOrElse(throw IllegalArgumentException(p.showDisplay()()))

  lazy val answer1: Int =
    patterns.map(reflectiveRowOrColumn).sum

  //Part 2

  def equalBut1(l1: Long, l2: Long): Boolean =
    val diff = l1 ^ l2
    val bitCount = Bits.countBits(diff)
    bitCount == 1

  @tailrec
  def startsWithBut1(list: List[Long], prefix: List[Long]): Boolean =
    list match
      case Nil => false
      case head::tail =>
        prefix match
          case Nil => false
          case phead :: ptail =>
            if equalBut1(head, phead) then
              tail.startsWith(ptail)
            else if head == phead then
              startsWithBut1(tail, ptail)
            else
              false

  @tailrec
  def reflectivePosBut1(list: List[Long], rev: List[Long], pos: Int, len: Int): Option[Int] =
    list match
      case Nil => None
      case head :: tail =>
        if (pos > 0) &&
          ((pos <= len / 2 && startsWithBut1(list, rev)) || (pos > len / 2 && startsWithBut1(rev, list)))
        then
          Some(pos)
        else
          reflectivePosBut1(tail, head::rev, pos + 1, len)

  def reflectiveRowBut1(bin: Pattern): Option[Int] =
    val longs = bin.rows.map{ a =>
      if a.length > 31 then
        throw IllegalArgumentException(s"$a .length == ${a.length}")
      val bi = BigInt.apply(a.mkString(""), 2)
      val l = bi.toLong
      if bi != l then
        throw IllegalArgumentException(s"$bi != $l")
      l
    }
    reflectivePosBut1(longs.toList, Nil, 0, longs.size)

  def reflectiveRowOrColumnBut1(p: Pattern): Int =
    reflectiveRowBut1(p)
      .map(_ * 100)
      .orElse(reflectiveRowBut1(p.transpose))
      .getOrElse(throw IllegalArgumentException(p.showDisplay()()))

  // 31604 -  high 31325 -  high
  lazy val answer2: Int =
    patterns.map(reflectiveRowOrColumnBut1).sum


  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
