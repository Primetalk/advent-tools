package org.primetalk.advent2020

import org.primetalk.advent.tools.{GraphUtils, Utils}

import scala.annotation.tailrec

/**
  * https://adventofcode.com/2020/day/7
  *  --- Day 7: Handy Haversacks ---
  *
  * You land at the regional airport in time for your next flight. In fact, it looks like you'll even have time to grab some food: all flights are currently delayed due to issues in luggage processing.
  *
  * Due to recent aviation regulations, many rules (your puzzle input) are being enforced about bags and their contents; bags must be color-coded and must contain specific quantities of other color-coded bags. Apparently, nobody responsible for these regulations considered how long they would take to enforce!
  *
  * For example, consider the following rules:
  *
  * light red bags contain 1 bright white bag, 2 muted yellow bags.
  * dark orange bags contain 3 bright white bags, 4 muted yellow bags.
  * bright white bags contain 1 shiny gold bag.
  * muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
  * shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
  * dark olive bags contain 3 faded blue bags, 4 dotted black bags.
  * vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
  * faded blue bags contain no other bags.
  * dotted black bags contain no other bags.
  *
  * These rules specify the required contents for 9 bag types. In this example, every faded blue bag is empty, every vibrant plum bag contains 11 bags (5 faded blue and 6 dotted black), and so on.
  *
  * You have a shiny gold bag. If you wanted to carry it in at least one other bag, how many different bag colors would be valid for the outermost bag? (In other words: how many colors can, eventually, contain at least one shiny gold bag?)
  *
  * In the above rules, the following options would be available to you:
  *
  *     A bright white bag, which can hold your shiny gold bag directly.
  *     A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
  *     A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
  *     A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
  *
  * So, in this example, the number of bag colors that can eventually contain at least one shiny gold bag is 4.
  *
  * How many bag colors can eventually contain at least one shiny gold bag? (The list of rules is quite long; make sure you get all of it.)
  *
  * Your puzzle answer was 289.
  * --- Part Two ---
  *
  * It's getting pretty expensive to fly these days - not because of ticket prices, but because of the ridiculous number of bags you need to buy!
  *
  * Consider again your shiny gold bag and the rules from the above example:
  *
  *     faded blue bags contain 0 other bags.
  *     dotted black bags contain 0 other bags.
  *     vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted black bags.
  *     dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black bags.
  *
  * So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags within it) plus 2 vibrant plum bags (and the 11 bags within each of those): 1 + 1*7 + 2 + 2*11 = 32 bags!
  *
  * Of course, the actual rules have a small chance of going several levels deeper than this example; be sure to count all of the bags, even if the nesting becomes topologically impractical!
  *
  * Here's another example:
  *
  * shiny gold bags contain 2 dark red bags.
  * dark red bags contain 2 dark orange bags.
  * dark orange bags contain 2 dark yellow bags.
  * dark yellow bags contain 2 dark green bags.
  * dark green bags contain 2 dark blue bags.
  * dark blue bags contain 2 dark violet bags.
  * dark violet bags contain no other bags.
  *
  * In this example, a single shiny gold bag must contain 126 other bags.
  *
  * How many individual bags are required inside your single shiny gold bag?
  *
  * Your puzzle answer was 30055.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  *
  */
object Day2007 extends Utils {
  import fastparse._
  import NoWhitespace._

  case class Bag(color: String)
  case class BagQuantity(count: Int, bag: Bag)
  case class Rule(bag: Bag, contents: List[BagQuantity])

  val input: IndexedSeq[String] = readResourceLines("day7.txt")

  def spaces[_ : P]: P[Unit] =
    P(" ".rep)

  def parseNoOtherBags[_ : P]: P[List[BagQuantity]] =
    P("no other bags".!).map(_ => Nil)

  def parseInt[_ : P]: P[Int] = P(
    CharPred(CharPredicates.isDigit).rep(1).!
      .map(_.toInt)
  )

  def bagKeyWord[_ : P]: P[Unit] =
    P("bags" | "bag")

  def word[_ : P]: P[String] = P(!bagKeyWord ~ CharPred(CharPredicates.isLetter).rep(1).! ~ spaces)

  def words[_ : P]: P[Seq[String]] =
    P((!bagKeyWord ~ word).rep(1))

  def bag[_ : P]: P[Bag] = P(words ~ bagKeyWord)
    .map(words => Bag(words.mkString(" ")))
  def bagQuantity[_ : P]: P[BagQuantity] = P(
    spaces ~ parseInt ~ spaces ~ bag
  ).map{ case (cnt, bag) => BagQuantity(cnt, bag)}

  def rule[_ : P]: P[Rule] = P(bag ~ spaces ~ "contain"~ spaces ~
    (parseNoOtherBags | bagQuantity.rep(min = 1, sep = ",")) ~ ".")
    .map{ case (b,lst) => Rule(b, lst.toList)}

  def parseRule(s: String): Rule =
    parse(s, rule(_)).get.value

  lazy val rules: List[Rule] =
    input.map(parseRule).toList

  lazy val edges: List[(Bag, Bag)] = for{
        r <- rules
        ch <- r.contents
      } yield (ch.bag, r.bag)

  lazy val parents: Map[Bag, List[Bag]] =
    edges
      .groupBy(_._1)
      .view.mapValues(_.map(_._2))
      .toMap

  val shinyGoldBag: Bag = Bag("shiny gold")
  // 34, 4, 122, 290, 289
  lazy val answer1: Int = {
    val connected = GraphUtils
      .collectConnectedComponent(parents
        .view.mapValues(_.toSet)
        .toMap
      )(shinyGoldBag)
    connected.size - 1
  }

  // Part 2
  lazy val children: Map[Bag, List[BagQuantity]] = {
    rules
      .map(r => (r.bag, r.contents))
      .toMap
  }

  @tailrec
  def countBags(toVisit: List[Bag], incomplete: Set[Bag] = Set(), knownAmounts: Map[Bag, Int] = Map()): Map[Bag, Int] = toVisit match {
    case ::(head, tail) =>
      if(knownAmounts.keySet.contains(head))
        countBags(tail, incomplete, knownAmounts)
      else {
        val ch = children.getOrElse(head, List())
        val newChildren = ch.filterNot(bq => knownAmounts.keySet.contains(bq.bag)).map(_.bag)
        newChildren match {
          case Nil =>
            countBags(tail, incomplete - head,
              knownAmounts.updated(head, ch.map(bq => (knownAmounts(bq.bag) + 1)*bq.count ).sum))
          case _ =>
            countBags(newChildren reverse_::: toVisit, incomplete, knownAmounts)
        }
      }
    case Nil =>
      if(incomplete.isEmpty)
        knownAmounts
      else {
        countBags(incomplete.toList, Set.empty, knownAmounts)
      }
  }
  // 175 - too low, 30055
  lazy val answer2: Int = {
    val amounts = countBags(List(shinyGoldBag))
    amounts(shinyGoldBag)
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }
}
