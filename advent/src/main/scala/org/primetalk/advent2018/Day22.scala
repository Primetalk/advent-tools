package org.primetalk.advent2018

import org.primetalk.advent.tools.{GraphUtils, Utils}
import org.primetalk.advent.tools.Geom2dUtils._
import org.primetalk.advent.tools.GraphUtils.{ReversePath, WeightedGraphAsFunction}

object Day22 extends Utils {

  lazy val inputTextFromResource : Iterator[String] =
    readResource("day22.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq

  type ErosionLevel = Int
  type RegionType = Char
  //rocky as ., wet as =, narrow as |
  val Rocky  = '.'
  val Wet    = '='
  val Narrow = '|'
  // regions which are either dominantly rocky, narrow, or wet (called its type
  val inputDepth = 4845
  val inputTarget: Position = (6, 770)
  val inputModulo = 20183

  type Instrument = Char
  val Torch: Instrument   = '8'
  val ClimbingGear: Instrument    = '*'
  val Neither: Instrument = '0'

  val tools = Seq(Torch, ClimbingGear, Neither)
  type ExtendedPosition = (Position, Instrument)

  def suitableTools(regionType: RegionType): Seq[Instrument] = regionType match {
    case Rocky => Seq(Torch, ClimbingGear)
    case Wet => Seq(ClimbingGear, Neither)
    case Narrow => Seq(Torch, Neither)
  }

  case class Eval(depth: Int, target: Position, modulo: Int){

    def evalGeologicalIndex(erosionLevels: Display[ErosionLevel], p: Position): Int = p match {
      case (0,0)            => 0
      case _ if p == target => 0
      case (x, 0)           => (x * 16807) % modulo
      case (0, y)           => (y * 48271) % modulo
      case (x, y)           => ( erosionLevels((x - 1, y)) * erosionLevels((x, y - 1)) ) % modulo
    }

    def erosionLevel(geologicalIndex: Int): Int =
      (geologicalIndex + depth) % modulo

    val regionTypes: Seq[RegionType] =
      Seq(Rocky, Wet, Narrow) // in order

    def getRiskLevel(rt: RegionType): Int = rt match {
      case Rocky => 0
      case Wet => 1
      case Narrow => 2
      case _ => ???
    }

    def renderErosionLevelsOfArea(size: Vector2d): Display[ErosionLevel] = {
      val erosionLevels = new Display[ErosionLevel](origin, size)()
      for{
        y <- erosionLevels.ys
        x <- erosionLevels.xs
      } {
        val p = (x, y)
        val i = evalGeologicalIndex(erosionLevels, p)
        erosionLevels(p) = erosionLevel(i)
      }
      erosionLevels
    }

    def totalRiskLevel(display: Display[RegionType]): Int = {
      val points = new Display[Nothing](origin, target + (1,1))().points // just a rectangle
      val values = points.map(display.apply).map(getRiskLevel)
      values.sum
    }
    // In order to find path we may need to look outside of the bounding rectangle.
    // This is the amount appended.
    val enlargeBy: Position = (46,1)//(150,150)//(25, 10) => 1050 // (46,1) => 1048

    val erosionLevels: Display[ErosionLevel] =
      renderErosionLevelsOfArea(target + enlargeBy)

    val regions: Display[RegionType] = erosionLevels.map(el => regionTypes(el % 3))

    regions(target) = Rocky

    def apply: Int =
      totalRiskLevel(regions)

    def graph(regions: Display[RegionType]): WeightedGraphAsFunction[ExtendedPosition, Int] = { case (currentPos, currentInstr) =>
      val nextPositionsWithTheSameInstrument =
        regions
          .adjacentPositions(currentPos)
          .filter(p => suitableTools(regions(p))
            .contains(currentInstr)
          )
      val nextInstrumentsInTheSamePosition = suitableTools(regions(currentPos)).filterNot(_ == currentInstr)
      nextPositionsWithTheSameInstrument.map(p => ((p, currentInstr), 1) ) ++
        nextInstrumentsInTheSamePosition.map(i => ((currentPos, i), 7))
    }

    def drawPath(d: Display[Char], p: ReversePath[ExtendedPosition]): Unit = {
      p.foreach{ case (pos, i) => d(pos) = i}
    }

    def timeToTarget: Int = {
      val gr = graph(regions)
      val initialExtendedPosition = (origin, Torch) // You start at 0,0 (the mouth of the cave) with the torch equipped
      import org.primetalk.advent.tools.TimeMeasurementUtils._
      val (time, paths) = measure(
        GraphUtils.findShortestPathsInWeightedGraph(
          gr, Set((target, Torch))
        )(Vector( (0, initialExtendedPosition :: Nil) ), lengthLimit = 1048 /*1200*/, distances = Map(initialExtendedPosition -> (0, initialExtendedPosition :: Nil)))
      ).report(t => println("time, ms: " + t))
      println("found: " + paths.size)
      println(paths.headOption.map(_.size))
      println(paths.headOption)
      drawPath(regions, paths.head) // destroying the regions
      println(regions.showDisplay()())
      time
    }
  }
  lazy val map0 = Eval(inputDepth, inputTarget, inputModulo)
  // 5531
  // 5400
  lazy val answer1: Long = {
    map0.apply
  }

  // Part 2
  // 1113 - too high
  // 1114
  // 1087 - cheating?
  // 1064 - too high
  // 1074 - corrected because of only suitable instruments can be switched to in the current region
  // 1058 - just guessed
  // 1063? - next guess
  // 1050 - incorrect answer. Currently it's what I have
  // 1050 - still incorrect...
  // 1048 - max X for the path is 52
  lazy val answer2: Long = {
    map0.timeToTarget
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
