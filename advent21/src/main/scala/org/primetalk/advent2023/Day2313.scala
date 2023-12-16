package org.primetalk.advent2023

import org.primetalk.advent3.tools.{IDisplay2D, Utils}

/**
  * https://adventofcode.com/2023/day/13
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

  def reflectivePosBut1(list: List[Long], rev: List[Long], pos: Int, len: Int): Option[Int] =
    if pos == 0 then
      reflectivePos(list.tail, list.head::rev, 1, len)
    else if pos == len then
      None
    else
      if (pos <= len / 2 && list.startsWith(rev)) || (pos > len / 2 && rev.startsWith(list)) then
        Some(pos)
      else
        reflectivePos(list.tail, list.head::rev, pos + 1, len)
  lazy val answer2: Int = 2

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
