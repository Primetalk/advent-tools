package org.primetalk.advent2018

import org.primetalk.advent.tools.SequenceUtils.{floyd, unfoldN}
import org.primetalk.advent.tools.Utils

/**
  * --- Day 21: Chronal Conversion ---
  *
  * You should have been watching where you were going, because as you wander the new North Pole base, you trip and fall into a very deep hole!
  *
  * Just kidding. You're falling through time again.
  *
  * If you keep up your current pace, you should have resolved all of the temporal anomalies by the next time the device activates. Since you have very little interest in browsing history in 500-year increments for the rest of your life, you need to find a way to get back to your present time.
  *
  * After a little research, you discover two important facts about the behavior of the device:
  *
  * First, you discover that the device is hard-wired to always send you back in time in 500-year increments. Changing this is probably not feasible.
  *
  * Second, you discover the activation system (your puzzle input) for the time travel module. Currently, it appears to run forever without halting.
  *
  * If you can cause the activation system to halt at a specific moment, maybe you can make the device send you so far back in time that you cause an integer underflow in time itself and wrap around back to your current time!
  *
  * The device executes the program as specified in manual section one and manual section two.
  *
  * Your goal is to figure out how the program works and cause it to halt. You can only control register 0; every other register begins at 0 as usual.
  *
  * Because time travel is a dangerous activity, the activation system begins with a few instructions which verify that bitwise AND (via bani) does a numeric operation and not an operation as if the inputs were interpreted as strings. If the test fails, it enters an infinite loop re-running the test instead of allowing the program to execute normally. If the test passes, the program continues, and assumes that all other bitwise operations (banr, bori, and borr) also interpret their inputs as numbers. (Clearly, the Elves who wrote this system were worried that someone might introduce a bug while trying to emulate this system with a scripting language.)
  *
  * What is the lowest non-negative integer value for register 0 that causes the program to halt after executing the fewest instructions? (Executing the same instruction multiple times counts as multiple instructions executed.)
  *
  * Your puzzle answer was 16311888.
  * --- Part Two ---
  *
  * In order to determine the timing window for your underflow exploit, you also need an upper bound:
  *
  * What is the lowest non-negative integer value for register 0 that causes the program to halt after executing the most instructions? (The program must actually halt; running forever does not count as halting.)
  *
  * Your puzzle answer was 1413889.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  *
  *
  * --- About the solution ---
  *
  * The actual code is rather small. We are not using real emulator of the device implemented previously.
  * What we did is manually recovered program structure and loops. Then we rewrote the code in a slightly
  * higher level code and managed to discover shorter procedures to some lengthy loops.
  * Knowing the conditions for loop termination we extracted the function that produced the next value for
  * comparison.
  * Then we use loop-detection algorithm implemented previously and find when the loop terminates.
  */
object Day21 extends Utils {
/*
PRE: r0 = ? rx = 0
#ip 4                       With go                        Structured loops
00: seti 123 0 3            r3 = 123                       r3 = 123
                                                           do {
01: bani 3 456 3            r3 = r3 & 456                    r3 &= 456
02: eqri 3 72 3             r3 = if(r3 == 72)
03: addr 3 4 4              if(r3 == 72) jump 05:          } until(r3 == 72)  // always correct. 73 == 123 & 456
04: seti 0 0 4              else jump 01:
05: seti 0 5 3              r3 = 0                         r3 = 0
                                                           do {
06: bori 3 65536 2          r2 = r3 | 65536                  r2 = r3 | 65536
07: seti 10736359 9 3       r3 = 10736359                    r3' = 10736359
                                                             do {
08: bani 2 255 1            r1 = r2 & 255                                                         r1 = r2 & 255
09: addr 3 1 3              r3 += r1                                                              r3'' = 10736359 + r1
10: bani 3 16777215 3       r3 &= 16777215                                                        r3''' = (10736359 + r1) & 16777215
11: muli 3 65899 3          r3 *= 65899                                                           r3'''' = r3''' * 65899
12: bani 3 16777215 3       r3 &= 16777215                                                        r3''''' = r3'''' & 16777215
                                                               r3 = ((10736359 + (r2 & 255)) & 16777215 * 65899) & 16777215
13: gtir 256 2 1            r1 = if(256 > r2)                                                     r1 = 256 > r2
14: addr 1 4 4              if(256 > r2) jump 16               if(r2 > 256)) {
15: addi 4 1 4              else jump 17
16: seti 27 2 4             jump 28:

                                                                  r2 = r2/256
17: seti 0 3 1              r1 = 0                                                                 r1 = 0
                                                                                                   do { // slow loop to divide by 256
18: addi 1 1 5              r5 = r1 +1
19: muli 5 256 5            r5 *= 256
20: gtrr 5 2 5              r5 = if(r5>r2)
21: addr 5 4 4              if(r5 > r2) jump 23                                                       if( (r1 + 1) * 256 > r2 ) ( r2 = r1; jump 08)
                                                                                                       r2 = r2/256
22: addi 4 1 4              else skip to 24:
23: seti 25 8 4             jump 26:
24: addi 1 1 1              r1 += 1                                                                   else r1 += 1
25: seti 17 6 4             jump 18:                                                                } loop until (r1 + 1) * 256 > r2
                                                                                                    r2 = r1
                                                                  close do loop
                                                               } while (r2 > 1) // equivalent to the check r2>256 before /256
26: setr 1 5 2              r2 = r1
27: seti 7 7 4              jump 08:
28: eqrr 3 0 1              r1 = if r3 == r0
29: addr 1 4 4              if(r3 == r0) jump 31: - halt
30: seti 5 1 4              else jump 06:                   } until (r3 == r0) // this is the only place where r0 is used.
                                                            halt
 */
  type Word = Long

  lazy val inputTextFromResource : Iterator[String] =
    readResource("day21.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toList

  /** Evaluates the next value of register r3 given it's current value. */
  def evalR3(r3: Word): Word = {
    def go(r2: Word, r3: Word): Word = {
      val r3r = (((r3 + (r2 & 255)) & 16777215L) * 65899L) & 16777215L
      if(r2 < 256)
        r3r
      else
        go(r2/256, r3r)
    }
    go(r3 | 65536L, 10736359L)
  }

  // 3411704
  // 133
  // 128
  // 10670720
  // 15808239
  // 16311888
  lazy val answer1: Long = {
    evalR3(0)
  }

  // Part 2
  // 9015838
  // 1797946
  // 1413889
  lazy val answer2: Long = {
    val initialValueOfR3 = 0L
    val (start, loop) = floyd(initialValueOfR3)()(evalR3)
//    println("(start, loop) = " + (start, loop) )
    val indexOfLastIterationInLoop = start + loop - 1
    unfoldN(initialValueOfR3, indexOfLastIterationInLoop)(evalR3)
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
