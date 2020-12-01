package org.primetalk.advent2019

import org.primetalk.advent.tools.Geom2dUtils.Position
import org.primetalk.advent.tools.{Display, Utils}

import scala.annotation.tailrec

/**
  * https://adventofcode.com/2019/day/13
  *
  * --- Day 13: Care Package ---
  *
  * As you ponder the solitude of space and the ever-increasing three-hour roundtrip for messages between you and Earth, you notice that the Space Mail Indicator Light is blinking. To help keep you sane, the Elves have sent you a care package.
  *
  * It's a new game for the ship's arcade cabinet! Unfortunately, the arcade is all the way on the other end of the ship. Surely, it won't be hard to build your own - the care package even comes with schematics.
  *
  * The arcade cabinet runs Intcode software like the game the Elves sent (your puzzle input). It has a primitive screen capable of drawing square tiles on a grid. The software draws tiles to the screen with output instructions: every three output instructions specify the x position (distance from the left), y position (distance from the top), and tile id. The tile id is interpreted as follows:
  *
  * 0 is an empty tile. No game object appears in this tile.
  * 1 is a wall tile. Walls are indestructible barriers.
  * 2 is a block tile. Blocks can be broken by the ball.
  * 3 is a horizontal paddle tile. The paddle is indestructible.
  * 4 is a ball tile. The ball moves diagonally and bounces off objects.
  *
  * For example, a sequence of output values like 1,2,3,6,5,4 would draw a horizontal paddle tile (1 tile from the left and 2 tiles from the top) and a ball tile (6 tiles from the left and 5 tiles from the top).
  *
  * Start the game. How many block tiles are on the screen when the game exits?
  *
  * Your puzzle answer was 270.
  * --- Part Two ---
  *
  * The game didn't run because you didn't put in any quarters. Unfortunately, you did not bring any quarters. Memory address 0 represents the number of quarters that have been inserted; set it to 2 to play for free.
  *
  * The arcade cabinet has a joystick that can move left and right. The software reads the position of the joystick with input instructions:
  *
  * If the joystick is in the neutral position, provide 0.
  * If the joystick is tilted to the left, provide -1.
  * If the joystick is tilted to the right, provide 1.
  *
  * The arcade cabinet also has a segment display capable of showing a single number that represents the player's current score. When three output instructions specify X=-1, Y=0, the third output instruction is not a tile; the value instead specifies the new score to show in the segment display. For example, a sequence of output values like -1,0,12345 would show 12345 as the player's current score.
  *
  * Beat the game by breaking all the blocks. What is your score after the last block is broken?
  *
  * Your puzzle answer was 12535.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day13 extends Utils with IntCodeComputer9 {

  val arcadeCabinetProgram: Program = Seq(1L,380,379,385,1008,2119,168858,381,1005,381,12,99,109,2120,1102,1,0,383,1102,0,1,382,21001,382,0,1,21002,383,1,2,21101,37,0,0,1105,1,578,4,382,4,383,204,1,1001,382,1,382,1007,382,37,381,1005,381,22,1001,383,1,383,1007,383,20,381,1005,381,18,
    1006,385,69,99,104,-1,104,0,4,386,3,384,1007,384,0,381,1005,381,94,107,0,384,381,1005,381,108,1106,0,161,107,1,392,381,1006,381,161,1101,0,-1,384,1106,0,119,1007,392,35,381,1006,381,161,1102,1,1,384,21001,392,0,1,21101,0,18,2,21101,0,0,3,21102,138,1,0,1105,1,549,1,392,384,392,
    21001,392,0,1,21102,18,1,2,21102,1,3,3,21102,1,161,0,1106,0,549,1101,0,0,384,20001,388,390,1,21001,389,0,2,21102,180,1,0,1105,1,578,1206,1,213,1208,1,2,381,1006,381,205,20001,388,390,1,20102,1,389,2,21102,1,205,0,1105,1,393,1002,390,-1,390,1102,1,1,384,21002,388,1,1,
    20001,389,391,2,21101,228,0,0,1105,1,578,1206,1,261,1208,1,2,381,1006,381,253,21002,388,1,1,20001,389,391,2,21101,253,0,0,1106,0,393,1002,391,-1,391,1102,1,1,384,1005,384,161,20001,388,390,1,20001,389,391,2,21101,279,0,0,1106,0,578,1206,1,316,1208,1,2,381,
    1006,381,304,20001,388,390,1,20001,389,391,2,21101,0,304,0,1106,0,393,1002,390,-1,390,1002,391,-1,391,1101,1,0,384,1005,384,161,21001,388,0,1,
    20102,1,389,2,21101,0,0,3,21102,338,1,0,1105,1,549,1,388,390,388,1,389,391,389,20102,1,388,1,21002,389,1,2,21102,4,1,3,21101,0,365,0,1105,1,549,1007,389,19,381,1005,381,75,104,-1,104,0,104,0,99,0,1,0,0,0,0,0,0,270,16,15,1,1,18,109,3,22102,1,-2,1,22102,1,-1,2,21101,0,0,3,21101,0,414,0,1105,1,549,
    21202,-2,1,1,22101,0,-1,2,21101,429,0,0,1106,0,601,2102,1,1,435,1,386,0,386,104,-1,104,0,4,386,1001,387,-1,387,1005,387,451,99,109,-3,2105,1,0,109,8,22202,-7,-6,-3,22201,-3,-5,-3,21202,-4,64,-2,2207,-3,-2,381,1005,381,492,21202,-2,-1,-1,22201,-3,-1,-3,2207,-3,-2,381,1006,381,481,21202,-4,8,-2,2207,-3,-2,381,1005,381,518,21202,-2,-1,-1,22201,-3,-1,-3,2207,-3,-2,381,1006,381,507,2207,-3,-4,381,1005,381,540,21202,-4,-1,-1,22201,-3,-1,-3,2207,-3,-4,381,1006,381,529,22102,1,-3,-7,109,-8,2105,1,0,109,4,1202,-2,37,566,201,-3,566,566,101,639,566,566,1202,-1,1,0,204,-3,204,-2,204,-1,109,-4,2105,1,0,109,3,1202,-1,37,593,201,-2,593,593,101,639,593,593,21002,0,1,-2,109,-3,2106,0,0,109,3,22102,20,-2,1,22201,1,-1,1,21102,1,373,2,21102,642,1,3,21101,0,740,4,21102,1,630,0,1106,0,456,21201,1,1379,-2,109,-3,2106,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,2,2,2,0,2,2,2,2,2,2,0,0,2,2,2,2,2,2,2,2,2,2,2,0,0,2,2,0,0,0,2,0,1,1,0,2,2,0,0,2,0,2,0,0,0,2,2,2,2,2,2,0,2,2,2,2,2,2,0,2,2,0,2,0,2,0,2,2,0,1,1,0,2,2,0,2,0,2,2,2,2,2,0,0,0,0,2,0,2,2,0,2,0,2,2,2,0,0,2,2,2,2,2,0,2,0,1,1,0,2,0,0,0,2,2,0,2,2,2,2,0,2,2,2,2,2,2,0,2,2,0,
    2,2,2,2,2,2,2,0,2,2,0,0,1,1,0,0,0,0,2,0,2,2,2,0,0,2,2,2,2,2,2,0,0,2,0,0,2,2,2,2,2,2,2,2,2,2,2,2,0,1,1,0,0,0,2,2,2,0,2,2,2,0,2,2,2,0,2,2,2,0,2,0,2,0,2,0,2,2,2,2,0,2,0,2,2,0,1,1,0,2,0,2,2,2,2,0,2,0,0,0,0,2,2,2,2,2,0,0,0,2,2,2,2,0,2,2,2,0,0,0,2,2,0,1,1,0,2,0,0,2,0,0,2,2,0,2,2,2,2,2,2,0,2,0,2,2,2,2,2,0,2,2,0,2,2,2,2,2,2,0,1,1,0,2,2,2,2,2,0,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,0,0,2,0,0,0,0,1,1,0,2,2,0,2,2,2,2,2,2,2,2,2,2,0,2,0,2,2,0,0,2,0,2,2,2,2,2,2,2,2,2,2,2,0,1,1,0,0,0,0,2,0,2,2,2,0,2,0,0,2,2,2,0,2,2,0,2,2,2,2,2,0,2,2,2,2,2,0,2,0,0,1,1,0,2,2,0,0,2,0,2,0,0,0,0,2,0,2,2,2,2,0,0,0,2,2,0,0,2,2,2,0,2,2,2,2,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,27,7,12,9,29,89,22,25,56,68,11,72,19,45,14,13,78,3,69,94,27,9,49,28,71,46,36,48,68,13,97,58,28,79,14,17,54,51,27,52,90,90,3,20,46,50,15,13,15,12,56,49,24,85,68,84,48,
    19,74,5,70,9,64,50,53,5,63,54,90,67,74,95,40,39,25,91,48,87,66,36,17,18,48,24,81,86,24,63,36,29,48,23,19,43,34,97,78,94,51,49,41,47,33,65,76,97,34,78,90,54,20,38,15,78,75,40,60,31,89,98,57,52,39,89,27,51,67,92,75,24,14,26,40,84,22,49,36,8,80,6,69,46,83,80,70,34,29,39,82,26,72,29,47,33,
    48,92,8,89,49,96,39,13,33,75,48,63,62,71,49,17,57,95,96,34,35,73,53,48,55,95,47,93,80,90,67,32,69,52,15,12,42,62,48,37,80,72,79,37,24,24,77,12,94,62,29,36,32,98,55,82,17,73,26,29,88,21,94,73,30,41,46,65,25,9,67,85,40,92,20,33,55,75,78,44,61,64,41,84,44,21,25,41,95,26,6,43,29,7,31,79,
    1,93,23,89,25,47,24,57,44,66,83,1,7,11,44,73,96,24,71,66,47,17,42,71,82,5,65,52,18,20,90,85,57,32,80,10,60,65,13,32,51,68,29,67,84,28,13,53,44,41,69,84,76,31,31,57,74,51,44,16,49,80,71,29,78,53,94,60,24,57,9,76,12,54,65,32,30,72,2,91,98,29,28,91,7,84,24,18,12,79,11,34,51,18,98,3,68,38,15,82,53,56,57,18,50,61,95,15,63,3,17,66,80,29,56,4,42,57,82,84,35,8,15,47,4,20,5,50,51,50,48,20,67,77,51,91,81,83,3,79,44,71,82,48,45,43,27,28,42,15,89,21,6,8,80,14,7,90,46,15,90,54,14,1,40,42,78,82,53,82,11,54,95,57,81,29,52,35,86,72,26,54,24,40,22,50,31,33,6,23,45,57,77,43,21,40,84,57,12,67,3,31,90,16,10,64,38,97,59,15,80,44,36,61,33,89,38,67,14,91,34,16,37,77,69,60,58,53,19,79,90,79,4,68,60,4,39,33,8,50,61,5,29,39,65,72,70,34,56,74,21,58,73,20,95,63,97,73,74,91,80,67,38,25,54,90,97,81,52,43,55,12,85,78,71,42,76,50,16,61,81,82,61,30,48,67,15,38,93,1,12,20,18,82,15,17,78,60,94,48,18,7,10,26,33,70,46,79,8,93,29,53,32,15,79,83,1,84,23,30,95,55,36,47,20,93,56,41,5,73,42,68,8,14,41,61,43,34,40,17,52,23,61,27,51,27,77,34,14,3,42,20,97,13,33,16,96,43,42,11,67,9,94,50,45,19,48,59,2,16,38,3,97,59,70,21,86,95,24,34,49,60,43,4,94,44,6,42,21,51,6,39,1,76,17,15,75,43,39,14,61,93,49,45,38,92,60,58,49,17,8,57,77,31,48,43,17,8,89,37,17,19,23,9,17,28,44,2,83,61,84,83,43,8,80,71,56,15,16,17,46,14,85,92,75,58,71,83,7,13,92,27,39,56,21,24,20,31,65,34,4,37,9,95,21,53,93,19,78,88,12,46,76,77,37,16,5,43,13,68,1,67,98,13,55,70,57,77,13,92,168858

  )

  case class Tile(i: Int, char: Char)

  val emptyTile: Tile = Tile(0, ' ') //  0 is an empty tile. No game object appears in this tile.
  val wallTile: Tile = Tile(1, '#') //  1 is a wall tile. Walls are indestructible barriers.
  val blockTile: Tile = Tile(2, '%') //  2 is a block tile. Blocks can be broken by the ball.
  val horizontalPaddle: Tile = Tile(3, '-') //  3 is a horizontal paddle tile. The paddle is indestructible.
  val ballTile: Tile = Tile(4, 'O') //  4 is a ball tile. The ball moves diagonally and bounces off objects.

  val tiles: List[Tile] = List(emptyTile, wallTile, blockTile, horizontalPaddle, ballTile)
  val tileMap: Map[Int, Tile] = tiles.map(t => (t.i, t)).toMap

  lazy val answer1: Int = {
    val display = new Display[Char]((0,0), (37,37))()
    display.fillAll(emptyTile.char)
    val s0 = State(ip = 0L, rb = 0L, new SimpleMemory(arcadeCabinetProgram), Nil)
    val s1 = runProgram(s0)
    val outputs = s1.outputs.reverse.sliding(3, 3)
    outputs.foreach{
      case List(x, y, i) =>
        display((x.toInt, y.toInt)) = tileMap(i.toInt).char
      case other =>
        throw new IllegalArgumentException(s"$other")
    }
    println(display.showDisplay()())
    display.values.count(_ == blockTile.char)
  }

  sealed trait Event
  case class BallMovedTo(x: Int, y: Int) extends Event
  case class PaddleMovedTo(x: Int, y: Int) extends Event
  case object GameOver extends Event

  sealed trait Strategy {
    val initialPosition: List[Word] = List(0L)
    // returns joystick position
    def handleEvent(event: Event): List[Word]
  }

  final class SimpleStrategy extends Strategy {
    var currentBallPosition: Position = (0,0)
    var currentPaddlePosition: Position = (0,0)

    def joystickPosition: Int =
      Integer.compare(currentBallPosition._1, currentPaddlePosition._1)

    override def handleEvent(event: Event): List[Word] = event match {
      case GameOver =>
        Nil
      case BallMovedTo(x,y) =>
        currentBallPosition = (x,y)
        List(joystickPosition)
      case PaddleMovedTo(x,y) =>
        currentPaddlePosition = (x,y)
        List(joystickPosition)
    }
  }
  final class Game {

    val display: Display[Char] = {
      val res = new Display[Char]((0,0), (37,37))()
      res.fillAll(emptyTile.char)
      res
    }

    private var s: State = {
      val memory = new SimpleMemory(arcadeCabinetProgram)
      memory(0) = 2L
      State(ip = 0L, rb = 0L, memory, List(0L))
    }

    private[Day13] var score: Word = 0L

    def updateDisplay(x: Word, y: Word, i: Word): Unit = {
      display((x.toInt, y.toInt)) = tileMap(i.toInt).char
    }

    def showDisplay(): Unit = {
      println("Score: " + score)
      println(display.showDisplay()())
    }

    def step(): Unit = {
      s = runUntilOrFinish(s => s.outputs.size == 3 || s.inputs.isEmpty)(s)
    }

    @tailrec
    def runUntilNextEvent(joystickPosition: List[Word]): Event = {
      s = s.copy(inputs = joystickPosition)
      step()
      if(s.ip == -1)
        GameOver
      else
        s.outputs match {
          case Nil =>
            runUntilNextEvent(joystickPosition)
          case score1 :: 0 :: -1 :: t =>
            score = score1
            s = s.copy(outputs = t)
            runUntilNextEvent(joystickPosition)
          case i :: y :: x :: t =>
            s = s.copy(outputs = t)
            updateDisplay(x,y,i)
            i match {
              case 4 =>
                showDisplay()
                BallMovedTo(x.toInt,y.toInt)
              case 3 =>
                showDisplay()
                PaddleMovedTo(x.toInt,y.toInt)
              case _ =>
                runUntilNextEvent(joystickPosition)
            }
          case _ =>
            throw new IllegalStateException(s"got not enough elements ${s.outputs}")
        }
    }
  }

  def play(game: Game, strategy: Strategy): Word = {

    @tailrec
    def loop(event: Event): Word = {
      strategy.handleEvent(event) match {
        case Nil => game.score
        case lst =>
          val nextEvent = game.runUntilNextEvent(lst)
          loop(nextEvent)
      }
    }
    val event = game.runUntilNextEvent(strategy.initialPosition)
    loop(event)
  }

  lazy val answer2: Long =  {
    val game = new Game
    val strategy = new SimpleStrategy
    play(game, strategy)
  }

  def main(args: Array[String]): Unit = {
//    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
