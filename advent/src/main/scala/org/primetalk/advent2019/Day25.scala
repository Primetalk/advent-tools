package org.primetalk.advent2019

import org.primetalk.advent.tools.{Display, GraphUtils, Utils}
import org.primetalk.advent.tools.Geom2dUtils._
import org.primetalk.advent.tools.GraphUtils.{PathInfo, ReversePath}

import scala.util.Random

/**
  * https://adventofcode.com/2019/day/25
  *
  * --- Day 25: Cryostasis ---
  *
  * As you approach Santa's ship, your sensors report two important details:
  *
  * First, that you might be too late: the internal temperature is -40 degrees.
  *
  * Second, that one faint life signature is somewhere on the ship.
  *
  * The airlock door is locked with a code; your best option is to send in a small droid to investigate the situation. You attach your ship to Santa's, break a small hole in the hull, and let the droid run in before you seal it up again. Before your ship starts freezing, you detach your ship and set it to automatically stay within range of Santa's ship.
  *
  * This droid can follow basic instructions and report on its surroundings; you can communicate with it through an Intcode program (your puzzle input) running on an ASCII-capable computer.
  *
  * As the droid moves through its environment, it will describe what it encounters. When it says Command?, you can give it a single instruction terminated with a newline (ASCII code 10). Possible instructions are:
  *
  * Movement via north, south, east, or west.
  * To take an item the droid sees in the environment, use the command take <name of item>. For example, if the droid reports seeing a red ball, you can pick it up with take red ball.
  * To drop an item the droid is carrying, use the command drop <name of item>. For example, if the droid is carrying a green ball, you can drop it with drop green ball.
  * To get a list of all of the items the droid is currently carrying, use the command inv (for "inventory").
  *
  * Extra spaces or other characters aren't allowed - instructions must be provided precisely.
  *
  * Santa's ship is a Reindeer-class starship; these ships use pressure-sensitive floors to determine the identity of droids and crew members. The standard configuration for these starships is for all droids to weigh exactly the same amount to make them easier to detect. If you need to get past such a sensor, you might be able to reach the correct weight by carrying items from the environment.
  *
  * Look around the ship and see if you can find the password for the main airlock.
  *
  * Your puzzle answer was 278664.
  * --- Part Two ---
  *
  * As you move through the main airlock, the air inside the ship is already heating up to reasonable levels. Santa explains that he didn't notice you coming because he was just taking a quick nap. The ship wasn't frozen; he just had the thermostat set to "North Pole".
  *
  * You make your way over to the navigation console. It beeps. "Status: Stranded. Please supply measurements from 49 stars to recalibrate."
  *
  * "49 stars? But the Elves told me you needed fifty--"
  *
  * Santa just smiles and nods his head toward the window. There, in the distance, you can see the center of the Solar System: the Sun!
  *
  * The navigation console beeps again.
  *
  * If you like, you can
  *
  * .
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day25 extends Utils with IntCodeComputer9  {

  lazy val inputTextFromResource : String =
    readResourceAsString("day25.txt")

  val asciiProgram: Seq[Long] = inputTextFromResource.split(",").map(_.toLong).toIndexedSeq

  sealed trait Instruction
  case object Wonder extends Instruction
  case class Move(dir: Direction) extends Instruction
  case class Take(item: String) extends Instruction
  case class Drop(item: String) extends Instruction
  case object Inventory extends Instruction

  case class RoomDescription(title: List[String], doors: List[Direction], items: List[String])
//  case object Prompt extends DroidOutput
//  case object HullBreach extends DroidOutput
  val directions: Map[Direction, String] = Map(
    Up -> "north",
    Down -> "south",
    Left -> "west",
    Right -> "east"
  )
  def instructionToString(instruction: Instruction): List[String] = instruction match {
    case Wonder => Nil
    case Move(dir) => List(directions(dir))
    case Take(item) => List("take " + item)
    case Drop(item) => List("drop " + item)
    case Inventory => List("inv")
  }
  def parseDroidOutput(s: List[String]): RoomDescription = {
    val s1 = s.dropWhile(_.isEmpty)
    val title = s1.takeWhile(_.nonEmpty)
    val s2 = s1.dropWhile(_.nonEmpty).dropWhile(_.isEmpty)

    RoomDescription(title, Nil, Nil )
//    case "Command?" => Prompt
//    case "== Hull Breach ==" => HullBreach
//    case _ => throw new IllegalArgumentException(s"Unexpected droid output: $s")
  }
  class AsciiComputer(program: Seq[Long]) {
    val id: Int = Random.nextInt()
    var state: State = State(ip = 0, rb = 0, new SimpleMemory[Word](program), Nil)
    var pos: Position = (0,0)
    var items: List[String] = Nil
    def enter(lines: List[String]): List[String] = {
//      lines.foreach(s => println("< " + s))
      val inputs = lines.map{_ + "\n"}.flatMap(_.toCharArray).map(_.toLong)
      state = state.copy(inputs = state.inputs ::: inputs)
      val s1 = runUntilBlockedOnInputOrFinish(state)
//      val s1 = runUntilOrFinish(state => state.outputs.contains('\n'.toLong))(state)
      state = s1.copy(outputs = Nil)
      val outputs = s1.outputs.reverse.map(_.toChar).mkString.split("\n").toList
      outputs.foreach(s => println("> " + s))
      outputs
    }
    def enterInstructions(instructions: List[Instruction]): RoomDescription = {
      parseDroidOutput(enter(instructions.flatMap(instructionToString)))
    }
    def deepCopy: AsciiComputer = {
      val res = new AsciiComputer(program)
      res.state = state.deepCopy
      res.pos = pos
      res.items = items
      res
    }
    def tryMovingTo(dir: Vector2d): Option[Position] = {
      enterInstructions(List(Move(dir), Move((0,0) - dir))) match {
//        case Prompt :: _ => Some(pos + dir)
//        case HullBreach :: _ => None
          // > - cake
        //> - prime number
        //> - mutex
        //> - dehydrated water
        //> - coin
        //> - manifold
        //> - candy cane
        //> - fuel cell
        case _ => None
      }
    }
  }

  val steps: List[String] =
    """
      |north
      |take candy cane
      |south
      |west
      |south
      |south
      |take coin
      |west
      |south
      |take prime number
      |north
      |east
      |north
      |east
      |take cake
      |west
      |north
      |east
      |south
      |take fuel cell
      |north
      |west
      |take mutex
      |south
      |// Crew Quarters!
      |// east
      |//north
      |//west
      |//south
      |//west //
      |//
      |south
      |west
      |take dehydrated water
      |east
      |north
      |north
      |east
      |//  Hull Breach
      |south
      |south
      |take manifold
      |north
      |north
      |west
      |south
      |east
      |north
      |west
      |south
      |""".stripMargin.split("\n")
      .filterNot(_.startsWith("//"))
      .toList
  //> - cake
  //> - mutex
  //> - coin
  //> - fuel cell
  lazy val answer1: Int = {
//    println(asciiProgram.filter(c =>  (c < 128 && c >= 32) || c == 10).map(_.toChar).mkString)
//    case class State(p: Position, items: )
//    type S1 = List[String]
//    type S2 = Position
//    def graph(n: (S1, S2)): List[(S1, S2)] = {
//      mainDirections.map{ dir => Move(dir) }
//    }
    val start = new AsciiComputer(asciiProgram)

    var currentDroid = start
    steps.foreach(input =>
      currentDroid.enter(List(input)))

    @scala.annotation.tailrec
    def repl: Nothing = {
      val in = scala.io.StdIn.readLine()
      currentDroid.enter(if(in.isEmpty) List() else List(in))
      repl
    }
    repl
//    def setStateAndLookAround: AsciiComputer => Seq[Position] = droid => {
//      currentDroid = droid
//      mainDirections.flatMap{ dir =>
//        val d1 = droid.deepCopy
//        d1.tryMovingTo(dir)
//      }
//    }
//    def newStateMovedTo: (AsciiComputer, Position) => AsciiComputer = (droid, pos) => {
//      val dir = pos - droid.pos
//      val droid2 = droid.deepCopy
//      val res = droid2.tryMovingTo(dir)
//      require(res.isDefined)
//      droid2
//    }
//    val (l, s, p) = GraphUtils.findAllShortestPaths5[AsciiComputer, Position](setStateAndLookAround, newStateMovedTo, _ => false)(
//      toVisit = Vector(((0,0), 0, Nil, start)),
//      distances = Map()
//    )
    1
  }

  // Part 2

  lazy val answer2: Long = {
    2
  }


  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }
}
