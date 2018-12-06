package org.primetalk.advent2017

import org.primetalk.advent.Utils

/**
  * --- Day 8: I Heard You Like Registers ---
  *
  * You receive a signal directly from the CPU. Because of your recent assistance with jump instructions, it would like you to compute the result of a series of unusual register instructions.
  *
  * Each instruction consists of several parts: the register to modify, whether to increase or decrease that register's value, the amount by which to increase or decrease it, and a condition. If the condition fails, skip the instruction without modifying the register. The registers all start at 0. The instructions look like this:
  *
  * b inc 5 if a > 1
  * a inc 1 if b < 5
  * c dec -10 if a >= 1
  * c inc -20 if c == 10
  *
  * These instructions would be processed as follows:
  *
  * Because a starts at 0, it is not greater than 1, and so b is not modified.
  * a is increased by 1 (to 1) because b is less than 5 (it is 0).
  * c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
  * c is increased by -20 (to -10) because c is equal to 10.
  *
  * After this process, the largest value in any register is 1.
  *
  * You might also encounter <= (less than or equal to) or != (not equal to). However, the CPU doesn't have the bandwidth to tell you what all the registers are named, and leaves that to you to determine.
  *
  * What is the largest value in any register after completing the instructions in your puzzle input?
  *
  * Your puzzle answer was 6828.
  * --- Part Two ---
  *
  * To be safe, the CPU also needs to know the highest value held in any register during this process so that it can decide how much memory to allocate to these operations. For example, in the above instructions, the highest value ever held was 10 (in register c after the third instruction was evaluated).
  *
  * Your puzzle answer was 7234.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day8 extends Utils {
  lazy val inputTextFromResource : Iterator[String] =
    readResource("day8.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq

  /**
    * b inc 5 if a > 1
    * a inc 1 if b < 5
    * c dec -10 if a >= 1
    * c inc -20 if c == 10
    */
  type Id = String
  type Operation = String // allowed

  val `>`: Operation = ">"
  val `<`: Operation = "<"
  val `>=`: Operation = ">="
  val `<=`: Operation = "<="
  val `==`: Operation = "=="
  val `!=`: Operation = "!="

  case class Condition(register: Id, op: Operation, boundary: Int)

  type Command = String
  val inc: Command = "inc"
  val dec: Command = "dec"
  case class RegisterCommand(register: Id, cmd: Command, argument: Int, condition: Condition)
  //b inc 5 if a > 1
  def parse(line: String): RegisterCommand = {
    val Seq(regId, cmd, argStr, _, condRegId, op, condArgStr) = line.split(' ').toSeq
    RegisterCommand(regId, cmd, argStr.toInt, Condition(condRegId, op, condArgStr.toInt))
  }

  lazy val commands: Seq[RegisterCommand] = lines.map(parse)

  type Gamma = Map[Id, Int]

  def check(gamma: Gamma)(condition: Condition): Boolean = condition match {
    case Condition(condRegId, ">", condArg) => gamma.getOrElse(condRegId, 0) > condArg
    case Condition(condRegId, "<", condArg) => gamma.getOrElse(condRegId, 0) < condArg
    case Condition(condRegId, ">=", condArg) => gamma.getOrElse(condRegId, 0) >= condArg
    case Condition(condRegId, "<=", condArg) => gamma.getOrElse(condRegId, 0) <= condArg
    case Condition(condRegId, "==", condArg) => gamma.getOrElse(condRegId, 0) == condArg
    case Condition(condRegId, "!=", condArg) => gamma.getOrElse(condRegId, 0) != condArg
  }

  def operation(cmd: Command): Int => Int => Int = cmd match {
    case "inc" => a => b => a + b
    case "dec" => a => b => a - b
  }

  def eval(gamma: Gamma)(command: RegisterCommand): Gamma = command match {
    case RegisterCommand(regId, cmd, arg, condition) if check(gamma)(condition) =>
      val old = gamma.getOrElse(regId, 0)
      val f = operation(cmd)
      val newValue = f(old)(arg)
      gamma.updated(regId, newValue)
    case _ => gamma
  }

  def runAll(gamma: Gamma = Map())(commands: Seq[RegisterCommand]): Gamma =
    commands.foldLeft(gamma)((g, c) => eval(g)(c))

  def answer1: Int = {
    val finalGamma = runAll()(commands)
    finalGamma.values.max
  }

  // Part 2
  def runAll2(gamma: Gamma = Map())(commands: Seq[RegisterCommand]): (Gamma, Int) =
    commands.foldLeft((gamma, Int.MinValue)){ case ((g, m), c) =>
      val g2 = eval(g)(c)
      (g2, math.max(m, g2.values.max))
    }

  def answer2: Int = {
    val (_, m)= runAll2()(commands)
    m
  }

  def main(args: Array[String]): Unit = {
    println(answer2)
  }
}
