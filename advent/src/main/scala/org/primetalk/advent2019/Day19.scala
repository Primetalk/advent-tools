package org.primetalk.advent2019

/**
  *
  * https://adventofcode.com/2019/day/19
  *
  * --- Day 19: Tractor Beam ---
  *
  * Unsure of the state of Santa's ship, you borrowed the tractor beam technology from Triton. Time to test it out.
  *
  * When you're safely away from anything else, you activate the tractor beam, but nothing happens. It's hard to tell whether it's working if there's nothing to use it on. Fortunately, your ship's drone system can be configured to deploy a drone to specific coordinates and then check whether it's being pulled. There's even an Intcode program (your puzzle input) that gives you access to the drone system.
  *
  * The program uses two input instructions to request the X and Y position to which the drone should be deployed. Negative numbers are invalid and will confuse the drone; all numbers should be zero or positive.
  *
  * Then, the program will output whether the drone is stationary (0) or being pulled by something (1). For example, the coordinate X=0, Y=0 is directly in front of the tractor beam emitter, so the drone control program will always report 1 at that location.
  *
  * To better understand the tractor beam, it is important to get a good picture of the beam itself. For example, suppose you scan the 10x10 grid of points closest to the emitter:
  *
  * X
  * 0->      9
  * 0#.........
  * |.#........
  *  v..##......
  * ...###....
  * ....###...
  * Y .....####.
  * ......####
  * ......####
  * .......###
  *  9........##
  *
  * In this example, the number of points affected by the tractor beam in the 10x10 area closest to the emitter is 27.
  *
  * However, you'll need to scan a larger area to understand the shape of the beam. How many points are affected by the tractor beam in the 50x50 area closest to the emitter? (For each of X and Y, this will be 0 through 49.)
  *
  * Your puzzle answer was 173.
  * --- Part Two ---
  *
  * You aren't sure how large Santa's ship is. You aren't even sure if you'll need to use this thing on Santa's ship, but it doesn't hurt to be prepared. You figure Santa's ship might fit in a 100x100 square.
  *
  * The beam gets wider as it travels away from the emitter; you'll need to be a minimum distance away to fit a square of that size into the beam fully. (Don't rotate the square; it should be aligned to the same axes as the drone grid.)
  *
  * For example, suppose you have the following tractor beam readings:
  *
  * #.......................................
  * .#......................................
  * ..##....................................
  * ...###..................................
  * ....###.................................
  * .....####...............................
  * ......#####.............................
  * ......######............................
  * .......#######..........................
  * ........########........................
  * .........#########......................
  * ..........#########.....................
  * ...........##########...................
  * ...........############.................
  * ............############................
  * .............#############..............
  * ..............##############............
  * ...............###############..........
  * ................###############.........
  * ................#################.......
  * .................########OOOOOOOOOO.....
  * ..................#######OOOOOOOOOO#....
  * ...................######OOOOOOOOOO###..
  * ....................#####OOOOOOOOOO#####
  * .....................####OOOOOOOOOO#####
  * .....................####OOOOOOOOOO#####
  * ......................###OOOOOOOOOO#####
  * .......................##OOOOOOOOOO#####
  * ........................#OOOOOOOOOO#####
  * .........................OOOOOOOOOO#####
  * ..........................##############
  * ..........................##############
  * ...........................#############
  * ............................############
  * .............................###########
  *
  * In this example, the 10x10 square closest to the emitter that fits entirely within the tractor beam has been marked O. Within it, the point closest to the emitter (the only highlighted O) is at X=25, Y=20.
  *
  * Find the 100x100 square closest to the emitter that fits entirely within the tractor beam; within that square, find the point closest to the emitter. What value do you get if you take that point's X coordinate, multiply it by 10000, then add the point's Y coordinate? (In the example above, this would be 250020.)
  *
  * Your puzzle answer was 6671097.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day19 extends IntCodeComputer9 {

  val program: Seq[Word] = Seq[Word](109,424,203,1,21102,11,1,0,1106,0,282,21101,18,0,0,1106,0,259,1202,1,1,221,203,1,21101,31,0,0,1105,1,282,21101,0,38,0,1106,0,259,20101,0,23,2,22101,0,1,3,21102,1,1,1,21102,57,1,0,1105,1,303,2102,1,1,222,21002,221,1,3,21001,221,0,2,21101,0,259,1,21101,80,0,0,1106,0,225,21101,0,149,2,21101,91,0,0,1106,0,303,1202,1,1,223,21002,222,1,4,21102,1,259,3,21101,225,0,2,21101,0,225,1,21101,118,0,0,1106,0,225,21001,222,0,3,21102,1,58,2,21102,1,133,0,1106,0,303,21202,1,-1,1,22001,223,1,1,21101,148,0,0,1105,1,259,1201,1,0,223,21002,221,1,4,20102,1,222,3,21101,21,0,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21102,195,1,0,105,1,109,20207,1,223,2,20102,1,23,1,21101,0,-1,3,21102,1,214,0,1106,0,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,2101,0,-4,249,22101,0,-3,1,21201,-2,0,2,22101,0,-1,3,21101,250,0,0,1106,0,225,22101,0,1,-4,109,-5,2106,0,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2105,1,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,22101,0,-2,-2,109,-3,2106,0,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,22101,0,-2,3,21101,0,343,0,1105,1,303,1105,1,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,22102,1,-4,1,21102,1,384,0,1105,1,303,1105,1,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,21202,1,1,-4,109,-5,2105,1,0
  )
  lazy val answer1: Word = {
    val beam = for {
      j <- 0 until 50
      i <- 0 until 50
      o <- getOutputs(program, List(i,j))
    } yield o
    beam.sum
  }

  // Part 2

  def beam(x: Int, y: Int): Boolean = {
    x >= 0 && y >= 0 &&
      getOutputs(program, List(x,y)).head == 1
  }

  val size = 100
  /** will return two additional points */
  def getDiagonalTopLeftAdd1(x: Int, y: Int, l: Int = 2): Seq[Boolean] = {
    for{
      i <- - l until size + l
    } yield beam(x + i, y + size - 1 - i)
  }

  def showNear(x: Int, y: Int, l: Int = 3): Unit = {
    println(s"Near ($x,$y)")

    for {
      j <- y - l to y + l
    } {
      for {
        i <- x - l to x + l
      } { print(if(beam(i,j)) '#' else '.')}
      println("")
    }
  }

  def show(seq: Seq[Boolean]): String = {
    new String(seq.map{
      case true => '#'
      case false => '.'
    }.toArray)
  }
  def startEnd(seq: Seq[Boolean]): (Int, Int) = {
    val n = seq.takeWhile(b => !b).size
    val l = seq.drop(n).takeWhile(identity).size
    (n, l)
  }
  lazy val answer2: Int = {
    val (x, y) = (667,1097)
    showNear(x + size - 1, y, 4)
    showNear(x, y + size - 1, 4)
    println(beam(x + size - 1, y), beam(x + size, y),beam(x + size - 1, y - 1))
    println(beam(x, y + size - 1), beam(x-1, y + size - 1), beam(x, y + size))
    val res = getDiagonalTopLeftAdd1(x,y) //650 360 = 59; 850 400 78 1090,662 99; 1099,667 = 100; 1097,666=100 1107, 673 = 101; 1099,668 = 100; 1097,667
    println(show(res))
    println(startEnd(res))

    x*10000+y
  }


  def main(args: Array[String]): Unit = {
//    println("Answer1: " + answer1)
    println("Answer2: " + answer2) // 10990667 10970666 11070673 10970667 10990668 6671097
    // ROTATED x,y!!!!
  }
}
