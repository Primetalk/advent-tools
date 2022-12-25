package org.primetalk.advent2022

import org.primetalk.advent3.tools.Utils
import org.primetalk.advent3.tools.Geom2dUtils._
import org.primetalk.advent3.tools.IDisplay2D
import org.primetalk.advent3.tools.NumberSequenceUtils
import org.primetalk.advent3.tools.GraphUtils
import org.primetalk.advent3.tools.GraphUtils._
import org.primetalk.advent3.tools.Priority
import cats.collections.Heap
import cats.kernel.Order

/**
  * https://adventofcode.com/2022/day/24
  */
object Day2224 extends Utils:

  val input = readThisObjectInput

  val testInput = """#.######
                    |#>>.<^<#
                    |#.<..<<#
                    |#>v.><>#
                    |#<^v^^>#
                    |######.#
                    |""".stripMargin

  val display = IDisplay2D.readCharDisplayFromString(input)
  val boundingRect  = display.rect.enlargeBy(-1,-1)
  val start = (1,0)
  val finish = display.rect.bottomRight - start

  require(display(finish) == '.')

  case class Blizzard(pos: Position, dir: Direction)

  val blizzards = display.points.flatMap(p =>
    ycharToDir.get(display(p)).map(dir => Blizzard(p, dir))
  )
  val blizzardIndex = boundingRect.points.map(p => p -> 
    blizzards.filter(b => b.pos._1 == p._1 || b.pos._2 == p._2)
  ).toMap.withDefault(_ => Seq())

  val maxTime = 1000
  case class State(pos: Position, time: Int, toVisit: List[Position]):
    def step: List[State] =
      toVisit match
        case head :: tail => 
          if time > maxTime 
          then List() 
          else
            val newPotentialPositions = moves.map(move => pos + move)
            val res = newPotentialPositions
              .filter(p => 
                boundingRect.contains(p) || p == start || p == finish)
              .filterNot(p =>
                blizzardIndex(p)
                  .exists(b => 
                    p == boundingRect.wrapIntoRange(b.pos + b.dir * (time+1))
                  )
              )
              .map(p => 
                  State(p, time + 1, if p == head then tail else toVisit))
            //println(s"$this -> $res")
            res
        case Nil =>
          List()

  val moves = List(Up, Down, Left, Right, (0,0))

  given Order[State] = 
    Order.by(s => 
      s.toVisit.size * 1000 + s.time * 10 + (if s.toVisit.isEmpty then 0 else manhattanDistance(s.pos, s.toVisit.head)))

  given Priority[State] = new Priority[State]:
    def apply(s: State): Long = -manhattanDistance(s.pos, finish)

  def generateAll(initialState: State): Long =
    object sh extends ShortestPathAlgorithms[State]:

      val graphAsFunction: GraphAsFunction[State] = _.step

      val isFinish = _.toVisit.isEmpty

      def find = 
        findAllShortestPaths7(
          toVisitSortedByPathInfoLength = List(PathInfo(0, List(initialState)))
        )
    sh.find._1
  // def generateAll(maxTime: Int = 20, maxStepsPerTime: Int = 100)(initialState: State): Long =
  //   GraphUtils.enumerateSearchSpace[State, (Int, Long)](
  //     next = _.step,
  //     aggregate = {
  //       case ((epoch, l), s) => 
  //         (epoch + 1, math.max(l, s.time))
  //     },
  //     eliminate = {
  //       case ((epoch,_), s) => 
  //         s.time < epoch / maxStepsPerTime
  //         || 
  //         s.time > maxTime
  //     }
  //   )(Heap(initialState), (0, 0))._2
  // 281
  lazy val answer1: Long =
    val initialState = State(start, 0, List(finish))
    generateAll(initialState)

  //Part 2
  lazy val answer2: Long =
    val initialState = State(start, 0, List(finish,start,finish))
    generateAll(initialState)

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
