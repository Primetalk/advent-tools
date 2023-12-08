package org.primetalk.advent2023

import org.primetalk.advent.tools.PrimeNumbers
import org.primetalk.advent3.tools.{NumberSequenceUtils, Utils}

import scala.annotation.tailrec

/**
  * https://adventofcode.com/2023/day/8
--- Day 8: Haunted Wasteland ---

You're still riding a camel across Desert Island when you spot a sandstorm quickly approaching. When you turn to warn the Elf, she disappears before your eyes! To be fair, she had just finished warning you about ghosts a few minutes ago.

One of the camel's pouches is labeled "maps" - sure enough, it's full of documents (your puzzle input) about how to navigate the desert. At least, you're pretty sure that's what they are; one of the documents contains a list of left/right instructions, and the rest of the documents seem to describe some kind of network of labeled nodes.

It seems like you're meant to use the left/right instructions to navigate the network. Perhaps if you have the camel follow the same instructions, you can escape the haunted wasteland!

After examining the maps for a bit, two nodes stick out: AAA and ZZZ. You feel like AAA is where you are now, and you have to follow the left/right instructions until you reach ZZZ.

This format defines each node of the network individually. For example:

RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)

Starting with AAA, you need to look up the next element based on the next left/right instruction in your input. In this example, start with AAA and go right (R) by choosing the right element of AAA, CCC. Then, L means to choose the left element of CCC, ZZZ. By following the left/right instructions, you reach ZZZ in 2 steps.

Of course, you might not find ZZZ right away. If you run out of left/right instructions, repeat the whole sequence of instructions as necessary: RL really means RLRLRLRLRLRLRLRL... and so on. For example, here is a situation that takes 6 steps to reach ZZZ:

LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)

Starting at AAA, follow the left/right instructions. How many steps are required to reach ZZZ?

Your puzzle answer was 13771.
--- Part Two ---

The sandstorm is upon you and you aren't any closer to escaping the wasteland. You had the camel follow the instructions, but you've barely left your starting position. It's going to take significantly more steps to escape!

What if the map isn't for people - what if the map is for ghosts? Are ghosts even bound by the laws of spacetime? Only one way to find out.

After examining the maps a bit longer, your attention is drawn to a curious fact: the number of nodes with names ending in A is equal to the number ending in Z! If you were a ghost, you'd probably just start at every node that ends with A and follow all of the paths at the same time until they all simultaneously end up at nodes that end with Z.

For example:

LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)

Here, there are two starting nodes, 11A and 22A (because they both end with A). As you follow each left/right instruction, use that instruction to simultaneously navigate away from both nodes you're currently on. Repeat this process until all of the nodes you're currently on end with Z. (If only some of the nodes you're on end with Z, they act like any other node and you continue as normal.) In this example, you would proceed as follows:

    Step 0: You are at 11A and 22A.
    Step 1: You choose all of the left paths, leading you to 11B and 22B.
    Step 2: You choose all of the right paths, leading you to 11Z and 22C.
    Step 3: You choose all of the left paths, leading you to 11B and 22Z.
    Step 4: You choose all of the right paths, leading you to 11Z and 22B.
    Step 5: You choose all of the left paths, leading you to 11B and 22C.
    Step 6: You choose all of the right paths, leading you to 11Z and 22Z.

So, in this example, you end up entirely on nodes that end in Z after 6 steps.

Simultaneously start on every node that ends with A. How many steps does it take before you're only on nodes that end with Z?

Your puzzle answer was 13129439557681.

Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2308 extends Utils:

  val lines: Seq[String] = readThisObjectInputLines

  def parseLine(line: String): (String, (String, String)) =
    line match
      case s"$from = ($l, $r)" => (from, (l, r))

  val map: Map[String, (String, String)] = lines.drop(2).map(parseLine).toMap

  private val instructionLine = lines.head
  type Instruction = LazyList[Char]
  private def instruction: Instruction = LazyList.from(instructionLine.toList) #::: instruction

  inline def next(current: String, instruction: Instruction): (String, Instruction) =
    val (l, r) = map(current)
    instruction match
      case head #:: tail =>
        head match
          case 'L' => (l, tail)
          case 'R' => (r, tail)

  @tailrec
  def countUntilTarget(current: String, instruction: LazyList[Char], target: String, count: Int = 0): Int =
    if current == target then
      count
    else
      val (n,in) = next(current, instruction)
      countUntilTarget(n, in, target, count + 1)

  lazy val answer1: Int =
    countUntilTarget("AAA", instruction, "ZZZ")



  //Part 2


//  def countUntilZ(current: List[String], instruction: LazyList[Char], count: Int = 0): Int =
//    if current.forall(_.last == 'Z') then
//      count
//    else
//      instruction match
//        case head #:: tail =>
//          countUntilZ({
//            current.map {
//              current =>
//                val (l, r) = map(current)
//                head match
//                  case 'L' => l
//                  case 'R' => r
//                  case _ => ???
//            }
//          },
//          tail, count + 1)
// countUntilZ(map.keys.filter(_.last == 'A').toList, instruction)
  private val starts = map.keys.filter(_.last == 'A').toList

  private def isTarget(s: String): Boolean =
    s.last == 'Z'

  inline def nextInt(current: String, instructionPos: Int): (String, Int) =
    val (l, r) = map(current)
    val nextId = instructionLine(instructionPos) match
      case 'L' => l
      case 'R' => r
    (nextId, (instructionPos + 1) % instructionLine.length)

  def from(s: String, instructionPos: Int): LazyList[String] =
    s #:: {
      val (n, in) = nextInt(s, instructionPos)
      from(n,in)
    }

  lazy val answer2: Long =
    val offsetsAndLoops = starts.map(start =>
      val (offset, loop) = NumberSequenceUtils.floydInt[(String, Int)]((start, 0))(_ == _)(p => nextInt(p._1,p._2))
      val cnt = offset + loop
      val loopElements = from(start, 0).take(cnt+1)
      val targetPositions = loopElements.zipWithIndex.filter(p => isTarget(p._1)).toList.map(_._2)//.map((a,b) => s"$a -> $b = 293*${b/293}")
      // Lucky match - points == List(loop) - single and position exactly equal to loop length
      require(targetPositions == List(loop), "Lucky match - targetPositions == List(loop) - single target && position exactly equal to loop length")
      (offset, loop, targetPositions)
    )
    val loopLengths = offsetsAndLoops.map(_._2.toLong)
    val lcm = PrimeNumbers.leastCommonMultipleN(loopLengths)
    //    val ((minOffset, minLoop, _),idx) = offsetsAndLoops.zipWithIndex.minBy(_._1._2)
    //    println(offsetsAndLoops.mkString("\n"))
    ////    val (minTarget, _) = NumberSequenceUtils.unfoldN[(String, LazyList[Char])](p => next(p._1,p._2))((start, instruction), 10L)
    //    //val earliests = starts.map(start => countUntilTarget(start, instruction, minTarget))
    //    println(s"least: $lcm")
    lcm

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
