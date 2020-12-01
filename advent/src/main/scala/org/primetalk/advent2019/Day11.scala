package org.primetalk.advent2019

import org.primetalk.advent.tools.{Display, Utils}
import org.primetalk.advent.tools.Geom2dUtils._
import scala.annotation.tailrec

/**
  * https://adventofcode.com/2019/day/11
  *
  * --- Day 11: Space Police ---
  *
  * On the way to Jupiter, you're pulled over by the Space Police.
  *
  * "Attention, unmarked spacecraft! You are in violation of Space Law! All spacecraft must have a clearly visible registration identifier! You have 24 hours to comply or be sent to Space Jail!"
  *
  * Not wanting to be sent to Space Jail, you radio back to the Elves on Earth for help. Although it takes almost three hours for their reply signal to reach you, they send instructions for how to power up the emergency hull painting robot and even provide a small Intcode program (your puzzle input) that will cause it to paint your ship appropriately.
  *
  * There's just one problem: you don't have an emergency hull painting robot.
  *
  * You'll need to build a new emergency hull painting robot. The robot needs to be able to move around on the grid of square panels on the side of your ship, detect the color of its current panel, and paint its current panel black or white. (All of the panels are currently black.)
  *
  * The Intcode program will serve as the brain of the robot. The program uses input instructions to access the robot's camera: provide 0 if the robot is over a black panel or 1 if the robot is over a white panel. Then, the program will output two values:
  *
  * First, it will output a value indicating the color to paint the panel the robot is over: 0 means to paint the panel black, and 1 means to paint the panel white.
  * Second, it will output a value indicating the direction the robot should turn: 0 means it should turn left 90 degrees, and 1 means it should turn right 90 degrees.
  *
  * After the robot turns, it should always move forward exactly one panel. The robot starts facing up.
  *
  * The robot will continue running for a while like this and halt when it is finished drawing. Do not restart the Intcode computer inside the robot during this process.
  *
  * For example, suppose the robot is about to start running. Drawing black panels as ., white panels as #, and the robot pointing the direction it is facing (< ^ > v), the initial state and region near the robot looks like this:
  *
  * .....
  * .....
  * ..^..
  * .....
  * .....
  *
  * The panel under the robot (not visible here because a ^ is shown instead) is also black, and so any input instructions at this point should be provided 0. Suppose the robot eventually outputs 1 (paint white) and then 0 (turn left). After taking these actions and moving forward one panel, the region now looks like this:
  *
  * .....
  * .....
  * .<#..
  * .....
  * .....
  *
  * Input instructions should still be provided 0. Next, the robot might output 0 (paint black) and then 0 (turn left):
  *
  * .....
  * .....
  * ..#..
  * .v...
  * .....
  *
  * After more outputs (1,0, 1,0):
  *
  * .....
  * .....
  * ..^..
  * .##..
  * .....
  *
  * The robot is now back where it started, but because it is now on a white panel, input instructions should be provided 1. After several more outputs (0,1, 1,0, 1,0), the area looks like this:
  *
  * .....
  * ..<#.
  * ...#.
  * .##..
  * .....
  *
  * Before you deploy the robot, you should probably have an estimate of the area it will cover: specifically, you need to know the number of panels it paints at least once, regardless of color. In the example above, the robot painted 6 panels at least once. (It painted its starting panel twice, but that panel is still only counted once; it also never painted the panel it ended on.)
  *
  * Build a new emergency hull painting robot and run the Intcode program on it. How many panels does it paint at least once?
  *
  * Your puzzle answer was 2056.
  * --- Part Two ---
  *
  * You're not sure what it's trying to paint, but it's definitely not a registration identifier. The Space Police are getting impatient.
  *
  * Checking your external ship cameras again, you notice a white panel marked "emergency hull painting robot starting panel". The rest of the panels are still black, but it looks like the robot was expecting to start on a white panel, not a black one.
  *
  * Based on the Space Law Space Brochure that the Space Police attached to one of your windows, a valid registration identifier is always eight capital letters. After starting the robot on a single white panel instead, what registration identifier does it paint on your hull?
  *
  * Your puzzle answer was GLBEPJZP.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day11 extends Utils with IntCodeComputer9 {

  val robotBrainProgram: Program = Seq(3,8,1005,8,321,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,29,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,50,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,73,1,1105,16,10,2,1004,8,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1002,8,1,103,1006,0,18,1,105,14,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,131,1006,0,85,1,1008,0,10,1006,0,55,2,104,4,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1001,8,0,168,2,1101,1,10,1006,0,14,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,196,1006,0,87,1006,0,9,1,102,20,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,1001,8,0,228,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,250,
    2,5,0,10,2,1009,9,10,2,107,17,10,1006,0,42,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,1001,8,0,287,2,102,8,10,1006,0,73,1006,0,88,1006,0,21,101,1,9,9,1007,9,925,10,1005,10,15,99,109,643,104,0,104,1,21102,1,387353256856L,1,21101,0,338,0,1105,1,442,21101,936332866452L,0,1,21101,349,0,0,1105,1,442,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,179357024347L,1,21101,0,396,0,1105,1,442,21102,1,29166144659L,1,21102,407,1,0,1105,1,442,3,10,104,0,104,0,3,10,104,0,104,0,
    21102,1,718170641252L,1,21102,430,1,0,1106,0,442,21101,825012151040L,0,1,21102,441,1,0,1106,0,442,99,109,2,21202,-1,1,1,21102,1,40,2,21102,1,473,3,21102,463,1,0,1105,1,506,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,468,469,484,4,0,1001,468,1,468,108,4,468,10,1006,10,500,1102,1,0,468,109,-2,2105,1,0,0,109,4,1202,-1,1,505,1207,-3,0,10,1006,10,523,21101,0,0,-3,22101,0,-3,1,21202,-2,1,2,21102,1,1,3,21102,1,542,0,1105,1,547,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,570,2207,-4,-2,10,1006,10,570,22102,1,-4,-4,
    1105,1,638,22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,0,589,0,1106,0,547,22102,1,1,-4,21101,1,0,-1,2207,-4,-2,10,1006,10,608,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,630,21202,-1,1,1,21102,630,1,0,105,1,505,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0
  )

  type RobotInput = Int// 0,1
  case class RobotOutput(color: Int, dir: Int)
  val whiteChar = '#'
  val blackChar = '.'
  val emptyChar = ' '

  case class RobotState(p: Position, angle: Vector2d, brain: State)

  def step(display: Display[Char])(robotState: RobotState): RobotState = robotState match {
    case RobotState(p: Position, angle: Vector2d, brain: State) =>
      val input =
        display(p) match {
          case `whiteChar` => 1
          case _ => 0
        }
      val s0 = brain.enqueue(input)
      val s1 = runUntilOutputOrFinish(s0)
      if(s1.ip == -1)
        RobotState(p, angle, s1)
      else {
        val (s2, colorOutput) = s1.dequeueOutput
        val color = colorOutput match {
          case 0 => blackChar
          case 1 => whiteChar
        }
        display(p) = color
        val s3 = runUntilOutputOrFinish(s2)
        val (s4, dirOutput) = s3.dequeueOutput
        val rotation = dirOutput match {
          case 0 => rotateLeft
          case 1 => rotateRight
        }
        val nextDir = rotation(angle)
        val nextPos = p + nextDir
        RobotState(nextPos, nextDir, s4)
      }
  }

  @tailrec
  def runRobotUntilHalt(display: Display[Char])(r0: RobotState): RobotState = {
    if(r0.brain.ip == -1)
      r0
    else {
      val r1 = step(display)(r0)
//      println()
//      println(display.showDisplay()())
      runRobotUntilHalt(display)(r1)
    }
  }
  lazy val answer1: Int = {
    val display = new Display[Char]((-80,-80), (161,161))()
    display.fillAll(emptyChar)
    val brain = State(ip = 0L, rb = 0L, new SimpleMemory(robotBrainProgram), Nil)
    val s0 = RobotState((0,0), Up, brain)
    val s1 = runRobotUntilHalt(display)(s0)
    display.values.count(_ != emptyChar)
  }

  lazy val answer2: Long =  {
    val display = new Display[Char]((0,-5), (43,6))()
    display.fillAll(emptyChar)
    display((0,0)) = whiteChar
    val brain = State(ip = 0L, rb = 0L, new SimpleMemory(robotBrainProgram), Nil)
    val s0 = RobotState((0,0), Up, brain)
    val s1 = runRobotUntilHalt(display)(s0)
    println(display.flipY.showDisplay()())
    2
  }

  def main(args: Array[String]): Unit = {
//    println("Answer1: " + answer1) // 1091; 2056
    println("Answer2: " + answer2) // GLBEPJZP
  }

}
