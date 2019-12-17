package org.primetalk.advent2019

import org.primetalk.advent.tools.{Display, GraphUtils}
import org.primetalk.advent.tools.Geom2dUtils._
import org.primetalk.advent.tools.GraphUtils.PathInfo

/**
  * https://adventofcode.com/2019/day/15
  *
  * --- Day 15: Oxygen System ---
  *
  * Out here in deep space, many things can go wrong. Fortunately, many of those things have indicator lights. Unfortunately, one of those lights is lit: the oxygen system for part of the ship has failed!
  *
  * According to the readouts, the oxygen system must have failed days ago after a rupture in oxygen tank two; that section of the ship was automatically sealed once oxygen levels went dangerously low. A single remotely-operated repair droid is your only option for fixing the oxygen system.
  *
  * The Elves' care package included an Intcode program (your puzzle input) that you can use to remotely control the repair droid. By running that program, you can direct the repair droid to the oxygen system and fix the problem.
  *
  * The remote control program executes the following steps in a loop forever:
  *
  * Accept a movement command via an input instruction.
  * Send the movement command to the repair droid.
  * Wait for the repair droid to finish the movement operation.
  * Report on the status of the repair droid via an output instruction.
  *
  * Only four movement commands are understood: north (1), south (2), west (3), and east (4). Any other command is invalid. The movements differ in direction, but not in distance: in a long enough east-west hallway, a series of commands like 4,4,4,4,3,3,3,3 would leave the repair droid back where it started.
  *
  * The repair droid can reply with any of the following status codes:
  *
  * 0: The repair droid hit a wall. Its position has not changed.
  * 1: The repair droid has moved one step in the requested direction.
  * 2: The repair droid has moved one step in the requested direction; its new position is the location of the oxygen system.
  *
  * You don't know anything about the area around the repair droid, but you can figure it out by watching the status codes.
  *
  * For example, we can draw the area using D for the droid, # for walls, . for locations the droid can traverse, and empty space for unexplored locations. Then, the initial state looks like this:
  *
  *
  *
  * D
  *
  *
  *
  * To make the droid go north, send it 1. If it replies with 0, you know that location is a wall and that the droid didn't move:
  *
  *
  * #
  * D
  *
  *
  *
  * To move east, send 4; a reply of 1 means the movement was successful:
  *
  *
  * #
  * .D
  *
  *
  *
  * Then, perhaps attempts to move north (1), south (2), and east (4) are all met with replies of 0:
  *
  *
  * ##
  * .D#
  * #
  *
  *
  * Now, you know the repair droid is in a dead end. Backtrack with 3 (which you already know will get a reply of 1 because you already know that location is open):
  *
  *
  * ##
  *    D.#
  * #
  *
  *
  * Then, perhaps west (3) gets a reply of 0, south (2) gets a reply of 1, south again (2) gets a reply of 0, and then west (3) gets a reply of 2:
  *
  *
  * ##
  * #..#
  *   D.#
  * #
  *
  * Now, because of the reply of 2, you know you've found the oxygen system! In this example, it was only 2 moves away from the repair droid's starting position.
  *
  * What is the fewest number of movement commands required to move the repair droid from its starting position to the location of the oxygen system?
  *
  * Your puzzle answer was 220.
  * --- Part Two ---
  *
  * You quickly repair the oxygen system; oxygen gradually fills the area.
  *
  * Oxygen starts in the location containing the repaired oxygen system. It takes one minute for oxygen to spread to all open locations that are adjacent to a location that already contains oxygen. Diagonal locations are not adjacent.
  *
  * In the example above, suppose you've used the droid to explore the area fully and have the following map (where locations that currently contain oxygen are marked O):
  *
  * ##
  * #..##
  * #.#..#
  * #.O.#
  * ###
  *
  * Initially, the only location which contains oxygen is the location of the repaired oxygen system. However, after one minute, the oxygen spreads to all open (.) locations that are adjacent to a location containing oxygen:
  *
  * ##
  * #..##
  * #.#..#
  * #OOO#
  * ###
  *
  * After a total of two minutes, the map looks like this:
  *
  * ##
  * #..##
  * #O#O.#
  * #OOO#
  * ###
  *
  * After a total of three minutes:
  *
  * ##
  * #O.##
  * #O#OO#
  * #OOO#
  * ###
  *
  * And finally, the whole region is full of oxygen after a total of four minutes:
  *
  * ##
  * #OO##
  * #O#OO#
  * #OOO#
  * ###
  *
  * So, in this example, all locations contain oxygen after 4 minutes.
  *
  * Use the repair droid to get a complete map of the area. How many minutes will it take to fill with oxygen?
  *
  * Your puzzle answer was 334.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day15 extends IntCodeComputer9 {

  val labyrinthProgram: Seq[Word] = Seq(
    3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,102,1,1034,1039,102,1,1036,1041,1001,1035,-1,1040,1008,1038,0,1043,
    102,-1,1043,1032,1,1037,1032,1042,1105,1,124,1002,1034,1,1039,1002,1036,1,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1106,0,124,1001,1034,-1,1039,1008,1036,0,1041,1002,1035,1,1040,1001,1038,0,1043,102,1,1037,1042,1106,0,124,1001,
    1034,1,1039,1008,1036,0,1041,101,0,1035,1040,1002,1038,1,1043,
    1002,1037,1,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,37,1032,1006,1032,165,1008,1040,33,1032,1006,1032,165,1102,1,2,1044,1105,1,224,
    2,1041,1043,1032,1006,1032,179,1102,1,1,1044,1106,0,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,
    1007,0,72,1044,1105,1,224,1101,0,0,1044,1105,1,224,
    1006,1044,247,101,0,1039,1034,1001,1040,0,1035,1001,1041,0,1036,102,1,1043,1038,1002,1042,1,1037,4,1044,1106,0,0,88,40,30,49,14,76,90,49,13,52,39,90,19,1,33,96,15,67,92,19,82,71,43,53,74,46,84,4,37,
    99,87,52,39,48,79,8,74,31,62,4,47,75,81,73,9,60,75,59,97,3,46,86,90,91,85,69,98,15,40,6,88,18,81,71,51,99,11,73,86,14,59,91,88,63,58,86,18,98,66,74,48,43,70,99,83,17,98,92,86,96,26,17,52,88,82,4,80,98,70,77,33,76,74,55,78,53,41,84,88,23,48,87,
    65,96,91,59,32,29,9,83,75,97,68,93,40,96,28,76,66,82,89,80,1,84,37,86,
    42,95,74,79,62,87,43,69,89,83,70,87,33,82,99,95,68,26,97,10,76,49,28,96,49,65,93,42,38,77,68,70,90,33,53,74,57,98,54,18,76,55,73,10,40,88,76,17,15,81,37,37,30,97,40,71,79,95,1,62,13,85,90,74,4,11,77,78,1,78,74,19,99,98,
    7,8,76,28,97,77,62,21,85,80,29,60,77,25,93,23,97,84,67,75,92,98,51,35,87,66,80,54,89,34,80,82,4,56,50,87,48,55,97,21,97,76,75,50,9,75,91,66,22,67,96,25,90,73,74,28,29,94,89,53,2,58,78,18,15,87,77,12,11,80,71,91,76,69,79,25,
    84,30,41,70,85,6,95,96,30,5,73,96,88,27,37,87,62,20,78,90,30,21,96,92,70,32,36,59,94,25,92,92,24,79,71,57,92,74,93,41,96,74,90,47,81,43,70,77,96,64,73,62,95,96,16,92,43,80,79,55,80,66,95,14,26,37,89,5,68,75,67,20,95,78,38,
    99,56,23,60,58,48,84,86,53,48,95,65,99,4,68,83,84,12,26,84,93,6,85,14,63,80,83,10,95,77,32,94,80,43,51,97,92,4,32,35,93,44,97,97,97,14,56,73,96,83,14,40,78,95,32,69,1,94,30,95,41,96,85,70,79,65,52,23,65,54,98,8,86,82,1,4,82,96,33,99,76,48,75,2,99,67,96,50,95,88,52,95,46,64,96,
    85,43,24,82,41,79,65,47,83,16,95,70,75,15,38,83,39,15,97,80,59,81,77,39,77,32,89,56,88,25,75,8,92,19,86,79,74,86,64,51,20,91,81,53,95,68,91,77,65,86,22,21,77,42,84,75,40,40,98,29,29,35,73,32,13,80,40,91,12,48,95,97,56,3,32,
    15,83,53,97,21,94,21,59,89,29,23,98,5,99,33,71,30,89,93,37,50,95,74,2,78,92,21,90,87,57,15,75,89,28,80,45,67,77,99,82,8,86,83,85,93,99,53,55,94,90,1,87,74,39,88,65,55,77,64,87,92,59,99,7,54,96,50,35,6,82,18,6,73,92,49,
    10,96,31,77,33,97,58,94,40,45,14,90,75,66,14,58,79,24,32,58,95,82,89,49,87,31,63,90,42,96,36,73,16,77,5,81,99,35,80,87,13,71,79,15,92,8,51,92,88,20,95,30,89,86,80,98,60,99,43,90,23,58,90,43,87,83,33,83,90,33,93,75,31,91,80,57,15,97,47,94,
    94,44,49,59,77,83,4,67,75,19,13,62,89,4,61,96,70,41,61,87,73,43,99,68,18,89,13,71,76,75,6,25,19,96,89,28,89,58,8,92,44,77,81,37,5,92,82,33,81,90,20,91,93,15,28,92,89,76,61,73,44,95,57,83,94,78,42,79,47,75,89,81,15,87,13,86,45,89,74,97,37,78,87,96,59,80,33,87,60,86,66,80,52,94,0,0,21,21,1,10,1,0,0,0,0,0,0
  )

  class Labyrinth {
    type NumDir = Word

    val north: NumDir = 1
    val south: NumDir = 2
    val west: NumDir = 3
    val east: NumDir = 4

    val mapDirs: Map[Vector2d, NumDir] = Map(Up -> north, Down -> south, Left -> west, Right -> east)

    type DroidResponse = Word

    val HitTheWall: DroidResponse = 0
    val Moved: DroidResponse = 1
    val MovedAndArrived: DroidResponse = 2

    class DroidState {

      var labyrinth: State = State(ip = 0, rb = 0, new SimpleMemory[Long](labyrinthProgram, 2000), Nil)
      var position: Position = (0,0)
      def step(input: NumDir): Word = {
        val s0 = labyrinth.copy(inputs = List(input))
        val s1 = runUntilOutputOrFinish(s0)
        labyrinth = s1
        if(s1.ip == -1)
          throw new IllegalStateException("Unexpected halt")
        else
          s1.outputs match {
            case output :: t =>
              labyrinth = labyrinth.copy(outputs = t)
              output
            case Nil =>
              throw new IllegalStateException("Unexpected empty output")
          }
      }

      def deepCopy: DroidState = {
        val res = new DroidState
        res.labyrinth = labyrinth.deepCopy
        res.position = position
        res
      }
    }
    private var droidState: DroidState = new DroidState
    def setDroidState(n: DroidState): Unit = {
      droidState = n
    }
    private val display = Display[Char]((-21,-19), (41,41))()
    display.fillAll(' ')
    private var targets: Set[Position] = Set()// we don't have targets yet
    display((0,0)) = '.'
    def tryMovingTo(dir: Vector2d): Option[Position] = {
      val ndir = mapDirs(dir)
      droidState.step(ndir) match {
        case HitTheWall =>
          display(droidState.position + dir) = '#'
          println("------")
          println(display.showDisplay()())
          None
        case Moved =>
          droidState.position = droidState.position + dir
          display(droidState.position) = '.'
          Some(droidState.position)
        case MovedAndArrived =>
          droidState.position = droidState.position + dir
          display(droidState.position) = 'O'
          println(display.showDisplay()())
          targets = targets + droidState.position
          Some(droidState.position)
      }
    }
    def lookAround: Seq[Position] = {
      mainDirections.flatMap{ dir =>
        val newPos = droidState.position + dir
        display(newPos) match {
          case ' ' =>
            val possiblePos = tryMovingTo(dir)
            possiblePos match {
              case Some(_) =>
                tryMovingTo((0,0) - dir)//stepping back
                Seq(newPos)
              case None =>
                Seq()
            }
          case '.' =>
            Seq(newPos)
          case '#' =>
            Seq()
          case 'O' =>
            Seq(newPos)
        }
      }
    }
    private def setStateAndLookAround(s: DroidState): Seq[(Int, Int)] = {
      setDroidState(s)
      lookAround
    }
    private def newStateMovedTo(s: DroidState, p: Position): DroidState = {
      val res = s.deepCopy
      setDroidState(res)
      val dir = p - s.position
      tryMovingTo(dir) match {
        case Some(_) =>
          res
        case None =>
          throw new IllegalStateException("Couldn't move to a position that has been visited before")
      }
    }
    @inline
    private def isFinish(position: Position): Boolean = targets.contains(position)//display(position) == 'O'
    def exploreLabyrinth(): Int = {
      val res = GraphUtils.findAllShortestPaths5[DroidState, Position](
        setStateAndLookAround,
        newStateMovedTo,
        isFinish
      )(
        toVisit = Vector(((0,0), 0, Nil, new DroidState)),
        distances = Map()
      )
      res._1
    }

    def goToOxygenStationState(): (Position, DroidState) = {
      val res = GraphUtils.findAllShortestPaths5[DroidState, Position](
        setStateAndLookAround,
        newStateMovedTo,
        isFinish
      )(
        toVisit = Vector(((0,0), 0, Nil, new DroidState)),
        distances = Map()
      )
      setDroidState(res._3.head._2)
      println("Oxygen station position: ${droidState.position}")
      (droidState.position, droidState.deepCopy)
    }
    def exploreTheMap(position: Position, droidState: DroidState): Int = {
      val res = GraphUtils.findAllShortestPaths5[DroidState, Position](
        setStateAndLookAround,
        newStateMovedTo,
        _ => false// we never find the results
      )(
        toVisit = Vector((position, 0, Nil, droidState)),
        distances = Map(position -> PathInfo(0, Nil) )
      )
      println(display.showDisplay()())
      res._2
    }
  }

  lazy val answer1: Int = {
    val l = new Labyrinth
    l.exploreLabyrinth()
  }

  // Part 2

  lazy val answer2: Int = {
    val l = new Labyrinth
    val (droidPosition, ds) = l.goToOxygenStationState()
    l.exploreTheMap(droidPosition, ds)
  }


  def main(args: Array[String]): Unit = {
//    println("Answer1: " + answer1)
    println("Answer2: " + answer2) //530//529 too high
  }
}
