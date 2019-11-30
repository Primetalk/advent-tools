package org.primetalk.advent2017

import fastparse._
import NoWhitespace._
import org.primetalk.advent.tools.Utils

object Day9 extends Utils {

  case class Group(children: List[Group] = List(), garbageLength: Int = 0)

  def garbage[_ : P]: P[String] =
    P(
      "<" ~
        (
          (!"!" ~ !">" ~ AnyChar).!.map(Some(_))
          |
          ("!" ~ AnyChar)          .map(_ => None)
        )
          .rep.map(_.flatten.mkString)
      ~ ">"
    )

  def group[_ : P]: P[Group] =
    P("{" ~ (garbage.map(Left(_)) | group.map(Right(_))).rep(sep = ",") ~ "}")
      .map{ children =>
        val groups = children.collect { case Right(grp) => grp }.toList
        val topLevelGarbageLength = children.collect { case Left(txt) => txt.length }.sum
        val innerGarbageLength = groups.map(_.garbageLength).sum
        Group(groups, topLevelGarbageLength + innerGarbageLength)
      }

  def topGroup[_ : P]: P[Group] = P(group ~ End)

  lazy val inputTextFromResource : Iterator[String] =
    readResource("day9.txt")

  lazy val input: String = inputTextFromResource.next()

  def score(g: Group): Int = {
    def go(toVisit: List[(Int, Group)], acc: Int): Int = toVisit match {
      case Nil => acc
      case (score, group) :: tail =>
        go(group.children.map((score + 1, _)) reverse_::: tail, acc + score)
    }
    go(List((1, g)), 0)
  }
  def parseSingleGroup(str: String): Group = {
    parse(str, topGroup(_)).get.value
  }
  // 1030
  // 7640
  def answer1: Int = {
    score(parseSingleGroup(input))
  }

  // Part 2
  // 4368
  def answer2: Int =
    parseSingleGroup(input).garbageLength

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }
}
