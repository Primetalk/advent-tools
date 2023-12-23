package org.primetalk.advent2023

import org.primetalk.advent3.tools.Utils
import org.primetalk.advent3.tools.intervals.Interval

/**
  * https://adventofcode.com/2023/day/19
--- Day 19: Aplenty ---

The Elves of Gear Island are thankful for your help and send you on your way. They even have a hang glider that someone stole from Desert Island; since you're already going that direction, it would help them a lot if you would use it to get down there and return it to them.

As you reach the bottom of the relentless avalanche of machine parts, you discover that they're already forming a formidable heap. Don't worry, though - a group of Elves is already here organizing the parts, and they have a system.

To start, each part is rated in each of four categories:

    x: Extremely cool looking
    m: Musical (it makes a noise when you hit it)
    a: Aerodynamic
    s: Shiny

Then, each part is sent through a series of workflows that will ultimately accept or reject the part. Each workflow has a name and contains a list of rules; each rule specifies a condition and where to send the part if the condition is true. The first rule that matches the part being considered is applied immediately, and the part moves on to the destination described by the rule. (The last rule in each workflow has no condition and always applies if reached.)

Consider the workflow ex{x>10:one,m<20:two,a>30:R,A}. This workflow is named ex and contains four rules. If workflow ex were considering a specific part, it would perform the following steps in order:

    Rule "x>10:one": If the part's x is more than 10, send the part to the workflow named one.
    Rule "m<20:two": Otherwise, if the part's m is less than 20, send the part to the workflow named two.
    Rule "a>30:R": Otherwise, if the part's a is more than 30, the part is immediately rejected (R).
    Rule "A": Otherwise, because no other rules matched the part, the part is immediately accepted (A).

If a part is sent to another workflow, it immediately switches to the start of that workflow instead and never returns. If a part is accepted (sent to A) or rejected (sent to R), the part immediately stops any further processing.

The system works, but it's not keeping up with the torrent of weird metal shapes. The Elves ask if you can help sort a few parts and give you the list of workflows and some part ratings (your puzzle input). For example:

px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}

The workflows are listed first, followed by a blank line, then the ratings of the parts the Elves would like you to sort. All parts begin in the workflow named in. In this example, the five listed parts go through the following workflows:

    {x=787,m=2655,a=1222,s=2876}: in -> qqz -> qs -> lnx -> A
    {x=1679,m=44,a=2067,s=496}: in -> px -> rfg -> gd -> R
    {x=2036,m=264,a=79,s=2244}: in -> qqz -> hdj -> pv -> A
    {x=2461,m=1339,a=466,s=291}: in -> px -> qkq -> crn -> R
    {x=2127,m=1623,a=2188,s=1013}: in -> px -> rfg -> A

Ultimately, three parts are accepted. Adding up the x, m, a, and s rating for each of the accepted parts gives 7540 for the part with x=787, 4623 for the part with x=2036, and 6951 for the part with x=2127. Adding all of the ratings for all of the accepted parts gives the sum total of 19114.

Sort through all of the parts you've been given; what do you get if you add together all of the rating numbers for all of the parts that ultimately get accepted?

Your puzzle answer was 374873.
--- Part Two ---

Even with your help, the sorting process still isn't fast enough.

One of the Elves comes up with a new plan: rather than sort parts individually through all of these workflows, maybe you can figure out in advance which combinations of ratings will be accepted or rejected.

Each of the four ratings (x, m, a, s) can have an integer value ranging from a minimum of 1 to a maximum of 4000. Of all possible distinct combinations of ratings, your job is to figure out which ones will be accepted.

In the above example, there are 167409079868000 distinct combinations of ratings that will be accepted.

Consider only your list of workflows; the list of part ratings that the Elves wanted you to sort is no longer relevant. How many distinct combinations of ratings will be accepted by the Elves' workflows?

Your puzzle answer was 122112157518711.

Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2319 extends Utils:

  val input: String = readThisObjectInput

  type Name = String

  sealed trait Target

  sealed trait FinalTarget extends Target
  case object Rejected extends FinalTarget

  case object Accepted extends FinalTarget
  case class OtherRule(name: Name) extends Target

  sealed trait Statement
  type Op = '<' | '>'
  case class Condition(f: Char, op: Op, boundary: Int, target: Target) extends Statement

  case class Unconditional(target: Target) extends Statement

  type Rule = List[Statement]

  def parseTarget(s: String): Target =
    s match
      case "A" => Accepted
      case "R" => Rejected
      case _ => OtherRule(s)
  def parseStatement(s: String): Statement =
    val a = s.split(':')
    if a.length == 1 then
      Unconditional(parseTarget(s))
    else
      val Array(r,t) = a
      val target = parseTarget(t)
      r match
        case s"$param<$boundary" => Condition(param.head, '<': Op, boundary.toInt, target)
        case s"$param>$boundary" => Condition(param.head, '>': Op, boundary.toInt, target)
  def parseRule(line: String): (Name, Rule) =
    line match
      case s"$name{$commaSeparatedList}" =>
        val rs = commaSeparatedList.split(',')
        (name, rs.map(parseStatement).toList)

  type Param = Char
  type Instance = Map[Param, Int]
  def parseInstance(line: String): Instance =
    line match
      case s"{$commaSeparatedList}" =>
        commaSeparatedList.split(',')
          .map {
            case s"$param=$value" =>
              (param.head, value.toInt)
          }
          .toMap

  val startingWorkflow: Name = "in"
  case class Input(rules: Map[Name, Rule], instances: List[Instance])

  def parseInput(input: String): Input =
    val Array(r, i) = input.split("\n\n")
    Input(
      r.split('\n').map(parseRule).toMap,
      i.split('\n').map(parseInstance).toList,
    )

  def operation(op: Op): (Int, Int) => Boolean =
    op match
      case '<' => _ < _
      case '>' => _ > _

  def runRule(rule: Rule, instance: Instance): Target =
    rule match
      case Nil => ???
      case head :: tail =>
        head match
          case Condition(f, op, boundary, target) =>
            val v = instance(f)
            if operation(op)(v, boundary) then
              target
            else
              runRule(tail, instance)
          case Unconditional(target) => target

  def execute(rules: Map[Name, Rule], instance: Instance, startAt: Name = startingWorkflow): FinalTarget =
    val in = rules(startAt)
    val t = runRule(in, instance)
    t match
      case target: FinalTarget => target
      case OtherRule(name) => execute(rules, instance, name)

  val Input(rules, instances) = parseInput(input)

  lazy val answer1: Int =
    instances
      .map{ i =>
        val t = execute(rules, i)
        (i, t)
      }
      .filter(_._2 == Accepted)
      .map(_._1.values.sum)
      .sum

  //Part 2
  val initialInterval = Interval(1,4000)
  type IntervalInstance = Map[Char, Interval]
  val initialInstance: IntervalInstance = "xmas".map(ch => ch -> initialInterval).toMap
  def runRule2(rule: Rule, instance: IntervalInstance, accum: List[(IntervalInstance, Target)] = Nil): List[(IntervalInstance, Target)] =
    rule match
      case Nil => ???
      case head :: tail =>
        head match
          case Condition(f, op, boundary, target) =>
            val interval = instance(f)
            val (intervalTrue, intervalFalse) = op match
              case '<' =>
                interval.divideLessThan(boundary)
              case '>' =>
                interval.divideGreaterThan(boundary)
            val newAccum = intervalTrue.toList.map(i => instance.updated(f, i) -> target) reverse_::: accum
            intervalFalse.fold(newAccum)(i => runRule2(tail, instance.updated(f,i), newAccum) )
          case Unconditional(target) =>
            (instance -> target) :: accum
  def execute2(rules: Map[Name, Rule], instance: IntervalInstance, startAt: Name = startingWorkflow): List[(IntervalInstance, FinalTarget)] =
    val in = rules(startAt)
    val ts = runRule2(in, instance)
    ts.flatMap( (instance, t) =>
      t match
        case target: FinalTarget => List(instance -> target)
        case OtherRule(name) => execute2(rules, instance, name)
    )

  lazy val answer2: BigInt =
    val allIntervals =
      execute2(rules, initialInstance)
    val accepted = allIntervals.filter(_._2 == Accepted)
    accepted
      .map(_._1.values
        .map(_.length).map(BigInt.apply)
        .product
      )
      .sum

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
