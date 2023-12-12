package org.primetalk.advent2023

import org.primetalk.advent2023.Day2312.RowWithQuestions
import org.primetalk.advent3.tools.Utils

/**
  * https://adventofcode.com/2023/day/12
  */
object Day2312 extends Utils:

  val lines: Seq[String] = readThisObjectInputLines

  type RowWithQuestions = List[Char]
  def parseLine(line: String): (RowWithQuestions, List[Int]) =
    val Array(row, ints) = line.split(' ')
    (row.toList: RowWithQuestions, ints.split(',').map(_.toInt).toList)

  def countArrangements(line: RowWithQuestions, groups: List[Int]): Int =
    line match
      case head :: tail =>
        inline def consumeGroup: Int =
          groups match
            case Nil =>
              0
            case group :: groupsTail =>
              val tail2 = tail.drop(group)
              val chars = tail.take(group)
              val init = chars.take(group - 1)
              if chars.size < group - 1 then
                0
              else
                if init.forall(c => c == '#' || c == '?') then
                  if chars.size == group - 1 then
                    if groupsTail.isEmpty then 1 else 0
                  else
                    val last = chars.last
                    if last == '.' || last == '?' then
                      countArrangements(tail2, groupsTail)
                    else
                      0
                else
                  0
        head match
          case '.' =>
            countArrangements(tail, groups)
          case '#' =>
            consumeGroup
          case '?' =>
            countArrangements(tail, groups) + consumeGroup
      case Nil =>
        if groups.isEmpty then
          1
        else
          0

  lazy val answer1: Int =
    val arrangements = lines
      .map(parseLine)
      .map(countArrangements)
    println(arrangements)
    arrangements.sum

  //Part 2
  val unfoldedTasks = lines
    .map(parseLine)
    .map{
      case (row, groups) =>
        (
        (1 to 5).map(_ => row.mkString("")).mkString("?").toList,
          (1 to 5).flatMap(_ => groups).toList
        )
    }

  /** Подсчитывает максимальное количество групп, которые могут поместиться в вопросики, за которыми сразу . . */
  def consumeGreedy(questionCount: Int, groups: List[Int]): Int = ???
  def countArrangements2(line: RowWithQuestions, groups: List[Int]): Int =
    line match
      case head :: tail =>
        inline def consumeGroup: Int =
          groups match
            case Nil =>
              0
            case group :: groupsTail =>
              val tail2 = tail.drop(group)
              val chars = tail.take(group)
              val init = chars.take(group - 1)
              if chars.size < group - 1 then
                0
              else if init.forall(c => c == '#' || c == '?') then
                if chars.size == group - 1 then
                  if groupsTail.isEmpty then 1 else 0
                else
                  val last = chars.last
                  if last == '.' || last == '?' then
                    countArrangements(tail2, groupsTail)
                  else
                    0
              else
                0

        head match
          case '.' =>
            countArrangements(tail, groups)
          case '#' =>
            consumeGroup
          case '?' =>
            val questionGroupLength = tail.takeWhile(_ == '?').size + 1
            val newTail = tail.dropWhile(_ == '?')
            val maxConsumedGroupCount = consumeGreedy(questionGroupLength, groups)
            countArrangements(newTail, groups) + consumeGroup
      case Nil =>
        if groups.isEmpty then
          1
        else
          0

  lazy val answer2: Int =
//    println(unfoldedTasks.mkString("\n"))
    val arrangements = unfoldedTasks
      .map(countArrangements2)
    println(arrangements)
    arrangements.sum

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
