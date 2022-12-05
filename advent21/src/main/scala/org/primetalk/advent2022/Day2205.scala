package org.primetalk.advent2022

import org.primetalk.advent3.tools.Utils

/**
  * https://adventofcode.com/2022/day/5
--- Day 5: Supply Stacks ---

The expedition can depart as soon as the final supplies have been unloaded from the ships. Supplies are stored in stacks of marked crates, but because the needed supplies are buried under many other crates, the crates need to be rearranged.

The ship has a giant cargo crane capable of moving crates between stacks. To ensure none of the crates get crushed or fall over, the crane operator will rearrange them in a series of carefully-planned steps. After the crates are rearranged, the desired crates will be at the top of each stack.

The Elves don't want to interrupt the crane operator during this delicate procedure, but they forgot to ask her which crate will end up where, and they want to be ready to unload them as soon as possible so they can embark.

They do, however, have a drawing of the starting stacks of crates and the rearrangement procedure (your puzzle input). For example:

    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2

In this example, there are three stacks of crates. Stack 1 contains two crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains three crates; from bottom to top, they are crates M, C, and D. Finally, stack 3 contains a single crate, P.

Then, the rearrangement procedure is given. In each step of the procedure, a quantity of crates is moved from one stack to a different stack. In the first step of the above rearrangement procedure, one crate is moved from stack 2 to stack 1, resulting in this configuration:

[D]        
[N] [C]    
[Z] [M] [P]
 1   2   3 

In the second step, three crates are moved from stack 1 to stack 3. Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates:

        [Z]
        [N]
    [C] [D]
    [M] [P]
 1   2   3

Then, both crates are moved from stack 2 to stack 1. Again, because crates are moved one at a time, crate C ends up below crate M:

        [Z]
        [N]
[M]     [D]
[C]     [P]
 1   2   3

Finally, one crate is moved from stack 1 to stack 2:

        [Z]
        [N]
        [D]
[C] [M] [P]
 1   2   3

The Elves just need to know which crate will end up on top of each stack; in this example, the top crates are C in stack 1, M in stack 2, and Z in stack 3, so you should combine these together and give the Elves the message CMZ.

After the rearrangement procedure completes, what crate ends up on top of each stack?

Your puzzle answer was GFTNRBZPF.
--- Part Two ---

As you watch the crane operator expertly rearrange the crates, you notice the process isn't following your prediction.

Some mud was covering the writing on the side of the crane, and you quickly wipe it away. The crane isn't a CrateMover 9000 - it's a CrateMover 9001.

The CrateMover 9001 is notable for many new and exciting features: air conditioning, leather seats, an extra cup holder, and the ability to pick up and move multiple crates at once.

Again considering the example above, the crates begin in the same configuration:

    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

Moving a single crate from stack 2 to stack 1 behaves the same as before:

[D]        
[N] [C]    
[Z] [M] [P]
 1   2   3 

However, the action of moving three crates from stack 1 to stack 3 means that those three moved crates stay in the same order, resulting in this new configuration:

        [D]
        [N]
    [C] [Z]
    [M] [P]
 1   2   3

Next, as both crates are moved from stack 2 to stack 1, they retain their order as well:

        [D]
        [N]
[C]     [Z]
[M]     [P]
 1   2   3

Finally, a single crate is still moved from stack 1 to stack 2, but now it's crate C that gets moved:

        [D]
        [N]
        [Z]
[M] [C] [P]
 1   2   3

In this example, the CrateMover 9001 has put the crates in a totally different order: MCD.

Before the rearrangement process finishes, update your simulation so that the Elves know where they should stand to be ready to unload the final supplies. After the rearrangement procedure completes, what crate ends up on top of each stack?

Your puzzle answer was VRQWPDSGP.

Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2205 extends Utils:

  val input = readThisObjectInput

  val Array(cratesStr, movesStr) = input.split("\n\n")

  case class Move(count: Int, from: Int, to: Int)

  def parseMove(line: String): Move =
    val Seq(count: Int, from: Int, to: Int) = parseAllIntsInString(line)
    Move(count, from, to)

  val cratesLines = cratesStr.split("\n")
  val initialMaxHeigh = cratesLines.length - 1
  val cratesCount = (cratesLines(0).length + 1)/4

  def parseLevel(cratesLine: String): Seq[Option[Char]] =
    (0 until cratesCount).map(index => 
      val char = cratesLine(index * 4 + 1)
      if char == ' ' then
        None
      else
        Some(char)
    )

  def parseLines: Array[List[Char]] =
    val indexed = cratesLines.init
      .map(parseLevel)
      .map(_.zipWithIndex)
      .flatten
      .groupBy(_._2)
      .view
      .mapValues(_.map(_._1).flatten.toList)
    indexed.toList
      .sortBy(_._1)
      .map(_._2)
      .toArray

  val moves = movesStr
    .split("\n")
    .map(parseMove)

  val stacks = IArray.unsafeFromArray(parseLines)

  def applyMove(stack: IArray[List[Char]], move: Move): IArray[List[Char]] =
    val from = stack(move.from - 1)
    val to = stack(move.to - 1)
    val cnt = move.count
    def moveCnt(from: List[Char], to: List[Char], cnt: Int): (List[Char], List[Char]) =
      if cnt == 0 then
        (from, to)
      else
        from match
          case head :: fromRes =>
            moveCnt(fromRes, head :: to, cnt - 1)
          case Nil =>
            ???

    val (fromRes, toRes) = moveCnt(from, to, cnt)

    stack
      .updated(move.from - 1, fromRes)
      .updated(move.to - 1, toRes)
    
  
  lazy val answer1: String =
    val result = moves.foldLeft(stacks)(applyMove)
    result.map(_.head).mkString("")

  //Part 2
  def applyMove2(stack: IArray[List[Char]], move: Move): IArray[List[Char]] =
    val from = stack(move.from - 1)
    val to = stack(move.to - 1)
    val cnt = move.count
    val movingPart = from.take(cnt)

    stack
      .updated(move.from - 1, from.drop(cnt))
      .updated(move.to - 1, movingPart ::: to)

  lazy val answer2: String =
    val result = moves.foldLeft(stacks)(applyMove2)
    result.map(_.head).mkString("")

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
