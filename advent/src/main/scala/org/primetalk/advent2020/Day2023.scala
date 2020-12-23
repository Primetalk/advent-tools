package org.primetalk.advent2020

import org.primetalk.advent.tools.{Circle, SequenceUtils}

import scala.annotation.tailrec

/**
  * https://adventofcode.com/2020/day/23
  * {{{
  * --- Day 23: Crab Cups ---
  *
  * The small crab challenges you to a game! The crab is going to mix up some cups, and you have to predict where they'll end up.
  *
  * The cups will be arranged in a circle and labeled clockwise (your puzzle input). For example, if your labeling were 32415, there would be five cups in the circle; going clockwise around the circle from the first cup, the cups would be labeled 3, 2, 4, 1, 5, and then back to 3 again.
  *
  * Before the crab starts, it will designate the first cup in your list as the current cup. The crab is then going to do 100 moves.
  *
  * Each move, the crab does the following actions:
  *
  *     The crab picks up the three cups that are immediately clockwise of the current cup. They are removed from the circle; cup spacing is adjusted as necessary to maintain the circle.
  *     The crab selects a destination cup: the cup with a label equal to the current cup's label minus one. If this would select one of the cups that was just picked up, the crab will keep subtracting one until it finds a cup that wasn't just picked up. If at any point in this process the value goes below the lowest value on any cup's label, it wraps around to the highest value on any cup's label instead.
  *     The crab places the cups it just picked up so that they are immediately clockwise of the destination cup. They keep the same order as when they were picked up.
  *     The crab selects a new current cup: the cup which is immediately clockwise of the current cup.
  *
  * For example, suppose your cup labeling were 389125467. If the crab were to do merely 10 moves, the following changes would occur:
  *
  * -- move 1 --
  * cups: (3) 8  9  1  2  5  4  6  7
  * pick up: 8, 9, 1
  * destination: 2
  *
  * -- move 2 --
  * cups:  3 (2) 8  9  1  5  4  6  7
  * pick up: 8, 9, 1
  * destination: 7
  *
  * -- move 3 --
  * cups:  3  2 (5) 4  6  7  8  9  1
  * pick up: 4, 6, 7
  * destination: 3
  *
  * -- move 4 --
  * cups:  7  2  5 (8) 9  1  3  4  6
  * pick up: 9, 1, 3
  * destination: 7
  *
  * -- move 5 --
  * cups:  3  2  5  8 (4) 6  7  9  1
  * pick up: 6, 7, 9
  * destination: 3
  *
  * -- move 6 --
  * cups:  9  2  5  8  4 (1) 3  6  7
  * pick up: 3, 6, 7
  * destination: 9
  *
  * -- move 7 --
  * cups:  7  2  5  8  4  1 (9) 3  6
  * pick up: 3, 6, 7
  * destination: 8
  *
  * -- move 8 --
  * cups:  8  3  6  7  4  1  9 (2) 5
  * pick up: 5, 8, 3
  * destination: 1
  *
  * -- move 9 --
  * cups:  7  4  1  5  8  3  9  2 (6)
  * pick up: 7, 4, 1
  * destination: 5
  *
  * -- move 10 --
  * cups: (5) 7  4  1  8  3  9  2  6
  * pick up: 7, 4, 1
  * destination: 3
  *
  * -- final --
  * cups:  5 (8) 3  7  4  1  9  2  6
  *
  * In the above example, the cups' values are the labels as they appear moving clockwise around the circle; the current cup is marked with ( ).
  *
  * After the crab is done, what order will the cups be in? Starting after the cup labeled 1, collect the other cups' labels clockwise into a single string with no extra characters; each number except 1 should appear exactly once. In the above example, after 10 moves, the cups clockwise from 1 are labeled 9, 2, 6, 5, and so on, producing 92658374. If the crab were to complete all 100 moves, the order after cup 1 would be 67384529.
  *
  * Using your labeling, simulate 100 moves. What are the labels on the cups after cup 1?
  *
  * Your puzzle answer was 49576328.
  * --- Part Two ---
  *
  * Due to what you can only assume is a mistranslation (you're not exactly fluent in Crab), you are quite surprised when the crab starts arranging many cups in a circle on your raft - one million (1000000) in total.
  *
  * Your labeling is still correct for the first few cups; after that, the remaining cups are just numbered in an increasing fashion starting from the number after the highest number in your list and proceeding one by one until one million is reached. (For example, if your labeling were 54321, the cups would be numbered 5, 4, 3, 2, 1, and then start counting up from 6 until one million is reached.) In this way, every number from one through one million is used exactly once.
  *
  * After discovering where you made the mistake in translating Crab Numbers, you realize the small crab isn't going to do merely 100 moves; the crab is going to do ten million (10000000) moves!
  *
  * The crab is going to hide your stars - one each - under the two cups that will end up immediately clockwise of cup 1. You can have them if you predict what the labels on those cups will be when the crab is finished.
  *
  * In the above example (389125467), this would be 934001 and then 159792; multiplying these together produces 149245887792.
  *
  * Determine which two cups will end up immediately clockwise of cup 1. What do you get if you multiply their labels together?
  *
  * Your puzzle answer was 511780369955.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  *
  * At this point, you should return to your Advent calendar and try another puzzle.
  *
  * Your puzzle input was 523764819.
  * }}}
  */
object Day2023 {

  val inputString = "523764819"
  val input: Circle[Char] = Circle.apply(inputString.toList:_*)
  val count = 9
  val maxChar = '9'
  def move(current: Circle[Char]): Circle[Char] = {
//    println(current)
    val circle2 = current.makeSureRight(5)
    val List(head, a,b,c, next) = circle2.takeRight(5)
    val circle3 = circle2.dropRight(4).insertRight(head)
    @tailrec
    def dest(h: Char): Char =
      if(h == '0')
        dest(maxChar)
      else if(h == a || h == b || h == c)
        dest((h - 1).toChar)
      else
        h

    val destination = dest((head - 1).toChar)
    val circle4 = circle3.moveRightUntil(_ == destination)
    circle4.dropRight(1)
      .prepend(destination, a, b, c)
      .moveRightUntil(_ == next)
  }
  // 589146732, 814957632, 49576328
  lazy val answer1: String = {
//    val last = SequenceUtils.unfoldN(Circle.apply("389125467":_*), 100)(move)
    val last = SequenceUtils.unfoldN(input, 100)(move)
    last.moveRightUntil(_ == '1').shakeToRight.right.tail.mkString("")
  }

  //Part 2,

  val maxValue = 1_000_000

  private class Node(val id: Int, var down: Node, var next: Node)

  private def build(elements: Array[Int]): Node = {
    val N = elements.length
    val nodesIndexedMinus1: Array[Node] = (1 to N)
      .map{ i =>
        new Node(i, down = null, next = null)
      }
      .toArray
    nodesIndexedMinus1.foreach{ n =>
      n.down = nodesIndexedMinus1(
        if(n.id == 1)
          N - 1
        else
          n.id - 2
      )
    }
    elements.indices.foreach{ i =>
      val node = nodesIndexedMinus1(elements(i) - 1)
      val next = nodesIndexedMinus1(elements(
        if(i == N - 1)
          0
        else
          i + 1
      ) - 1)
      node.next = next
    }
    nodesIndexedMinus1(elements(0) - 1)
  }
  private def move4(current: Node): Node = {
    val head: Node = current
    val a = head.next
    val b = a.next
    val c = b.next
    val next = c.next
    head.next = next
    @tailrec
    def dest(h: Node): Node =
      if((h eq a) || (h eq b) || (h eq c))
        dest(h.down)
      else
        h
    val destination = dest(head.down)
    c.next = destination.next
    destination.next = a
    next
  }
  @tailrec
  private def findDownwards(current: Node, pred: Node => Boolean): Node =
    if(pred(current))
      current
    else
      findDownwards(current.down, pred)

  def build5(elements: Array[Int]): Array[Int] = {
    val N = elements.length
    val next = new Array[Int](N)
    (0 until N).foreach{ i =>
      next(elements(i) - 1) = elements(if( i < N - 1) i + 1 else 0) - 1
    }
    next
  }

  lazy val answer2: Long = {
    //    val inputString = "389125467" // test string
    val input2: Node = build((inputString.toList.map(_ - '0') ++ (10 to maxValue)).toArray)
    val last = SequenceUtils.unfoldN(input2, 10_000_000)(move4)
    val one = findDownwards(last, _.id == 1)
    val a = one.next
    val b = a.next
    a.id.toLong * b.id.toLong
  }

  lazy val answer2_2: Long = {

    val elements = (inputString.toList.map(_ - '0') ++ (10 to maxValue)).toArray
    //    val inputString = "389125467" // test string
    val next = build5(elements)
    def move5(current: Int): Int = {
      val head = current
      val a = next(head)
      val b = next(a)
      val c = next(b)
      val next1 = next(c)
      next(head) = next1
      def down(head: Int) = if(head > 0) head - 1 else next.length - 1
      @tailrec
      def dest(h: Int): Int =
        if((h == a) || (h == b) || (h == c))
          dest(down(h))
        else
          h
      val destination = dest(down(head))
      next(c) = next(destination)
      next(destination) = a
      next1
    }
    SequenceUtils.unfoldN(elements(0) - 1, 10_000_000)(move5)
    val a = next(0)
    val b = next(a)
    (a + 1).toLong * (b + 1).toLong
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
    println("Answer2_2: " + answer2_2)
  }

}
