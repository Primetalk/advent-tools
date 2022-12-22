package org.primetalk.advent2022

import org.primetalk.advent3.tools.Utils
import org.primetalk.advent3.tools.IDisplay2D
import org.primetalk.advent3.tools.Geom2dUtils
import org.primetalk.advent3.tools.Geom2dUtils._
import org.primetalk.advent3.tools.ParsingUtils
import org.primetalk.advent3.tools.ParsingUtils.parseUnsafe
import cats.parse.Parser
import org.primetalk.advent3.tools.Display2D
import scala.collection.immutable.ArraySeq

/**
  * https://adventofcode.com/2022/day/22
--- Day 22: Monkey Map ---

The monkeys take you on a surprisingly easy trail through the jungle. They're even going in roughly the right direction according to your handheld device's Grove Positioning System.

As you walk, the monkeys explain that the grove is protected by a force field. To pass through the force field, you have to enter a password; doing so involves tracing a specific path on a strangely-shaped board.

At least, you're pretty sure that's what you have to do; the elephants aren't exactly fluent in monkey.

The monkeys give you notes that they took when they last saw the password entered (your puzzle input).

For example:

        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5

The first half of the monkeys' notes is a map of the board. It is comprised of a set of open tiles (on which you can move, drawn .) and solid walls (tiles which you cannot enter, drawn #).

The second half is a description of the path you must follow. It consists of alternating numbers and letters:

    A number indicates the number of tiles to move in the direction you are facing. If you run into a wall, you stop moving forward and continue with the next instruction.
    A letter indicates whether to turn 90 degrees clockwise (R) or counterclockwise (L). Turning happens in-place; it does not change your current tile.

So, a path like 10R5 means "go forward 10 tiles, then turn clockwise 90 degrees, then go forward 5 tiles".

You begin the path in the leftmost open tile of the top row of tiles. Initially, you are facing to the right (from the perspective of how the map is drawn).

If a movement instruction would take you off of the map, you wrap around to the other side of the board. In other words, if your next tile is off of the board, you should instead look in the direction opposite of your current facing as far as you can until you find the opposite edge of the board, then reappear there.

For example, if you are at A and facing to the right, the tile in front of you is marked B; if you are at C and facing down, the tile in front of you is marked D:

        ...#
        .#..
        #...
        ....
...#.D.....#
........#...
B.#....#...A
.....C....#.
        ...#....
        .....#..
        .#......
        ......#.

It is possible for the next tile (after wrapping around) to be a wall; this still counts as there being a wall in front of you, and so movement stops before you actually wrap to the other side of the board.

By drawing the last facing you had with an arrow on each tile you visit, the full path taken by the above example looks like this:

        >>v#    
        .#v.    
        #.v.    
        ..v.    
...#...v..v#    
>>>v...>#.>>    
..#v...#....    
...>>>>v..#.    
        ...#....
        .....#..
        .#......
        ......#.

To finish providing the password to this strange input device, you need to determine numbers for your final row, column, and facing as your final position appears from the perspective of the original map. Rows start from 1 at the top and count downward; columns start from 1 at the left and count rightward. (In the above example, row 1, column 1 refers to the empty space with no tile on it in the top-left corner.) Facing is 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^). The final password is the sum of 1000 times the row, 4 times the column, and the facing.

In the above example, the final row is 6, the final column is 8, and the final facing is 0. So, the final password is 1000 * 6 + 4 * 8 + 0: 6032.

Follow the path given in the monkeys' notes. What is the final password?

Your puzzle answer was 95358.
--- Part Two ---

As you reach the force field, you think you hear some Elves in the distance. Perhaps they've already arrived?

You approach the strange input device, but it isn't quite what the monkeys drew in their notes. Instead, you are met with a large cube; each of its six faces is a square of 50x50 tiles.

To be fair, the monkeys' map does have six 50x50 regions on it. If you were to carefully fold the map, you should be able to shape it into a cube!

In the example above, the six (smaller, 4x4) faces of the cube are:

        1111
        1111
        1111
        1111
222233334444
222233334444
222233334444
222233334444
        55556666
        55556666
        55556666
        55556666

You still start in the same position and with the same facing as before, but the wrapping rules are different. Now, if you would walk off the board, you instead proceed around the cube. From the perspective of the map, this can look a little strange. In the above example, if you are at A and move to the right, you would arrive at B facing down; if you are at C and move down, you would arrive at D facing up:

        ...#
        .#..
        #...
        ....
...#.......#
........#..A
..#....#....
.D........#.
        ...#..B.
        .....#..
        .#......
        ..C...#.

Walls still block your path, even if they are on a different face of the cube. If you are at E facing up, your movement is blocked by the wall marked by the arrow:

        ...#
        .#..
     -->#...
        ....
...#..E....#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

Using the same method of drawing the last facing you had with an arrow on each tile you visit, the full path taken by the above example now looks like this:

        >>v#    
        .#v.    
        #.v.    
        ..v.    
...#..^...v#    
.>>>>>^.#.>>    
.^#....#....    
.^........#.    
        ...#..v.
        .....#v.
        .#v<<<<.
        ..v...#.

The final password is still calculated from your final position and facing from the perspective of the map. In this example, the final row is 5, the final column is 7, and the final facing is 3, so the final password is 1000 * 5 + 4 * 7 + 3 = 5031.

Fold the map into a cube, then follow the path given in the monkeys' notes. What is the final password?

Your puzzle answer was 144361.

Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2222 extends Utils:

  val input = readThisObjectInput
  // val input = """        ...#
  //               |        .#..
  //               |        #...
  //               |        ....
  //               |...#.......#
  //               |........#...
  //               |..#....#....
  //               |..........#.
  //               |        ...#....
  //               |        .....#..
  //               |        .#......
  //               |        ......#.
  //               |
  //               |10R5L5R10L4R5L5
  //               |""".stripMargin

  val Array(m, p) = input.split("\n\n")
  
  val strangeMap = Display2D.readCharDisplay(ArraySeq.unsafeWrapArray(m.split("\n")), ' ')

  sealed trait Command
  case class Forward(s: Int) extends Command
  case object TurnLeft extends Command
  case object TurnRight extends Command
  
  def parsePath(p: String): List[Command] = 
    val forward = ParsingUtils.positiveNumber.map(Forward.apply)
    val left = Parser.char('L').as(TurnLeft)
    val right = Parser.char('R').as(TurnRight)
    val command = forward | left | right
    given commands: Parser[List[Command]] = command.rep.map(_.toList)
    p.trim.parseUnsafe[List[Command]]

  val path = parsePath(p)

  case class State(pos: Position = (0, strangeMap.maxY), dir: Direction = Geom2dUtils.Right)

  def runPath(next: State => State)(path: List[Command])(s: State): State =
    // println(s)
    // println(strangeMap.showDisplay()())
    //println(s)
    path match
      case head :: tail => 
        runPath(next)(tail)(
          head match
            case Forward(cnt) => 
              def go1(state: State = s, cnt: Int = cnt, prevState: State = s): State =
                //println(s"  $pos - ($cnt) - $prevPos")
                cnt match
                  case 0 => prevState
                  case _ =>
                    val s2 = next(state)
                    strangeMap.apply(s2.pos) match
                      case ' ' => go1(s2, cnt, prevState) // NB! do not decrecemt cnt
                      case '.' | '*' => 
                        // strangeMap(s2.pos) = '*'
                        go1(s2, cnt - 1, s2)
                      case '#' => prevState
                  
              go1()
            case TurnLeft =>
              s.copy(dir = rotateRight.apply(s.dir))
            case TurnRight =>
              s.copy(dir = rotateLeft.apply(s.dir))
        )
      case Nil =>
        s

  val lines = readThisObjectInputLines
  // 95356 95358
  lazy val answer1: Int =
    val next = (s: State) => s.copy(pos = strangeMap.wrapIntoRange(s.pos + s.dir))
    val State((x, y), dir) = runPath(next)(path)(State())
    val n = dir match  //math.round(dir.flipY.theta/90.0).toInt
      case Geom2dUtils.Right => 0
      case Down => 1
      case Geom2dUtils.Left => 2
      case Up => 3
    ((strangeMap.maxY-y) + 1)*1000 + (x + 1) * 4 + n

  case class Edge(a: Char, b: Char):
    def reverse = Edge(b, a)
    override def toString() = String(Array(a,b))

  // letters in the face name should be placed from top left clockwise
  case class Face(name: String, rect: Rectangle):

    val vertices = name.toSet

    val AB = Edge(name(0), name(1))
    val BC = Edge(name(1), name(2))
    val CD = Edge(name(2), name(3))
    val DA = Edge(name(3), name(0))

    val dirEdge = List(
      (YUp, AB),
      (YRight, BC),
      (YDown, CD),
      (YLeft, DA),
    )

    val edgeByDir = dirEdge.toMap

    val edges = mainDirections.map(edgeByDir).toSet

    val dirByEdge = dirEdge.map(_.swap).toMap

    lazy val edgeSegment = mainDirections.map(edgeByDir)
      .map(edge => edge ->Rect(rect).edgeSegment(dirByEdge(edge)))
      .toMap
      
    def relative(edge: Edge, pos: Position): Position =
      edgeSegment(edge).relative(pos)
    def fromRelative(edge: Edge, pos: Position): Position =
      edgeSegment(edge).fromRelative(pos)
  //Part 2
  // 113363 low
  //  60251 ?? 
  // 144361 good
  val size = (50,50)
  val faces = List(
    Face("ABCD", Rectangle((50,0), size)),
    Face("BFGC", Rectangle((100,0), size)),
    Face("DCGH", Rectangle((50,50), size)),
    Face("HGFE", Rectangle((50,100), size)),
    Face("DHEA", Rectangle((0,100), size)),
    Face("AEFB", Rectangle((0,150), size)),
  )
  val startPos = (50,0)
  // val size = (4,4)
  // val faces = List(
  //   Face("ABCD", Rectangle((8,0), size)),
  //   Face("BAEF", Rectangle((0,4), size)),
  //   Face("ADHE", Rectangle((4,4), size)),
  //   Face("DCGH", Rectangle((8,4), size)),
  //   Face("HGFE", Rectangle((8,8), size)),
  //   Face("GCBF", Rectangle((12,8), size)),
  // )
  // val startPos = (8,0)
  var debug: Set[String] = Set()

  def next(s: State): State =
    val nextPos = s.pos + s.dir
    if faces.exists(_.rect.contains(nextPos)) then
      s.copy(pos = nextPos)
    else
      val oldFace = faces.find(_.rect.contains(s.pos))
        .getOrElse{
          throw IllegalArgumentException(s"couldn't find old Face at pos ${s.pos}")
        }
      val oldEdge = oldFace.edgeByDir(s.dir)
      val newEdge = oldEdge.reverse
      val newFace = faces.find(_.edges.contains(newEdge)).getOrElse(???)
      val newDir = newFace.dirByEdge(newEdge).reverse
      val oldEdgeSegment = oldFace.edgeSegment(oldEdge)
      val oldEdgeSegmentReversed = oldEdgeSegment.reverse
      val newPosRelativeToOldEdgeReversed = oldEdgeSegmentReversed.relative(nextPos)

      val newEdgeSegment = newFace.edgeSegment(newEdge)
      val newPosP1 = newEdgeSegment.fromRelative(newPosRelativeToOldEdgeReversed)
      val newPos = newPosP1 - newDir // step back to start from edge itself
      val newFace2 = faces.find(_.rect.contains(newPos))
        .getOrElse{
          throw IllegalArgumentException(s"couldn't find new Face at new pos ${newPos}")
        }
      if newFace.name != newFace2.name then
        throw IllegalArgumentException(s"invalid face $newFace2 found. expected $newFace")
      if strangeMap.isWithinRange(newPos) then
        val msg = s"$oldEdge${ydirToChar(s.dir)} ==> $newEdge${ydirToChar(newDir)}"
        //println(msg)
        debug = debug + msg
        State(newPos, newDir)
      else
        throw new IllegalStateException(s"$newPos is outside")

  lazy val answer2: Int =
    val State((x, y), dir) = runPath(next)(path)(State(pos = startPos))
    // println(debug)
    // println(debug.size)
    val n = dir match  //math.round(dir.flipY.theta/90.0).toInt
      case Right => 0
      case Down => 3
      case Left => 2
      case Up => 1
    // println(s"$x,$y; ${dirToChar(dir)}")
    ((y) + 1)*1000 + (x + 1) * 4 + n


  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
