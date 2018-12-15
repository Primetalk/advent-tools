package org.primetalk.advent

import Geom2dUtils._
import org.primetalk.advent.GraphUtils._
//{GraphAsFunction, PathInfo, ReversePath, findAllShortestPaths3}

object Day15 extends Utils {

  implicit val ro: Ordering[Position] = readingOrdering

  lazy val inputTextFromResource : String =
    readResourceAsString("day15.txt")

  def readMazeWithUnits(lines: Seq[String]): Display[Char] = {
    val size = (lines.head.length, lines.length) // 32*32
    val a = lines.map(_.toCharArray).toArray
    val d = Display[Char](origin, size)(Some(() => a))
    d
  }

  // Each unit, either Goblin or Elf, has 3 attack power and starts with 200 hit points.
  case class GameUnit(id: Int, kind: Char, position: Position,
    attackPower: Int = 3, hitPoints: Int = 200) {
    def isAlive: Boolean =
      hitPoints > 0
  }

  def findAllUnits(d: Display[Char]): Seq[GameUnit] = {
    val seq = for{
      p <- d.points
      c = d(p)
      if c == 'E' || c == 'G'
    } yield GameUnit(0, c, p)
    seq.zipWithIndex.map{ case (u, i) => u.copy(id = i) }
  }

  def readMaze(lines: Seq[String]): Display[Char] = {
    val size = (lines.head.length, lines.length) // 32*32
    val a = lines.map(_.replace('E', '.').replace('G', '.').toCharArray).toArray
    val d = Display[Char](origin, size)(Some(() => a))
    d
  }

  def convertMazeToGraph(d: Display[Char], occupiedPositions: Set[Position]): GraphAsFunction[Position] = p => {
//    if(d.isWithinRange(p)) {
      mainDirections
        .map( _ + p)
        .filterNot(occupiedPositions.contains)
        .filter(d.isWithinRange)
        .filter{ pp =>
          val c = d.apply(pp)
          c != '#'
        }
//    } else Seq()
  }

  type Id = Int

  def adjacent(p: Position): Seq[Position] =
    mainDirections.map(_ + p)

  case class State(immutableMaze: Display[Char], units: Map[Id, GameUnit], round: Int) {

    lazy val graph: GraphAsFunction[Position] =
      convertMazeToGraph(immutableMaze, units.values.map(_.position).toSet)

    /** Only empty cells are considered.*/
    def inRangeForMove(u: GameUnit): Seq[Position] =
      graph(u.position)

    /** One unit's turn. */
    def move(self: GameUnit, inRangeOfAllTargets: Set[Position]): State = {
      val (_, shortestPaths: Seq[ReversePath[Position]]) =
        findAllShortestPaths3(graph, self.position, inRangeOfAllTargets)
      if(shortestPaths.isEmpty)
        this
      else {
        val reversedPaths = shortestPaths.sortBy(_.head)
        val targetPosition = reversedPaths.head.head
        val reversedPaths2 = reversedPaths.filter(_.head == targetPosition)
        val paths = reversedPaths2.map(_.reverse)
        val selectedPath = paths.minBy(_.head)
        val firstStep = selectedPath.head
        copy(units = units.updated(self.id, self.copy(position = firstStep)))
      }
    }

    def attack(self: GameUnit): State = {
      val selfRange = adjacent(self.position)
      val targetsInRange =
        units.values
          .filter(_.kind != self.kind)
          .filter(t => selfRange.contains(t.position))
      if(targetsInRange.isEmpty)
        this //
      else {
        val target = targetsInRange.toSeq.minBy(u => (u.hitPoints, u.position)) // ?? Might not use correct ordering.
        val updatedTarget = target
          .copy(hitPoints = target.hitPoints - self.attackPower)
        copy(units =
          units
            .updated(target.id, updatedTarget)
            .filter(_._2.isAlive)) // killed units DIE
      }
    }

    def turn(self: GameUnit): State = {
      val targets = units.values.filter(_.kind != self.kind)
      if(targets.isEmpty)
        this
      else {
        val inRangeOfAllTargets: Set[Position] =
          targets.flatMap(t => adjacent(t.position)).toSet
        val s2 =
          if (inRangeOfAllTargets.contains(self.position) || inRangeOfAllTargets.isEmpty)
            this
          else {
            val withoutWallsOrUnits = inRangeOfAllTargets
              .filter(p => immutableMaze(p) == '.')
              .filterNot(p => units.exists(_._2.position == p))
            if(withoutWallsOrUnits.isEmpty)
              this
            else
              move(self, withoutWallsOrUnits)
          }
        val self2 = s2.units(self.id)
        s2.attack(self2)
      }
    }

    def isBattleCompleted: Boolean =
      units.values
        .filter(_.isAlive)
        .map(_.kind)
        .toSet.size == 1

    def totalHitPoints: Int = units.values.map(_.hitPoints).sum
    def outcome: Int = totalHitPoints * round

    def showWithUnits(): Unit = {
      println()
      println("After round " + round)
      val mapByY = units.values.map(u => (u.position._2, u)).groupBy(_._1).toMap.mapValues(_.map(_._2).toSet)
      for{
        y <- immutableMaze.ys
      } {
        val line = immutableMaze.array(y).toVector.toArray
        val unitsOnLine = mapByY.getOrElse(y, Set())
        unitsOnLine.foreach(c => line(c.position._1) = c.kind)
        println(new String(line))
      }
      println(units.values.toSeq.sortBy(_.position).map(_.toString).mkString("\n"))
    }

  }

  def round(s: State): State = {
//    System.gc() // todo: get rid
    s.showWithUnits()
    // the order in which units take their turns within a round is
    // the reading order of their starting positions in that round
    val unitIds = s.units.values.filter(_.isAlive).toSeq.sortBy(_.position).map(_.id)
    val resultState = unitIds
      .foldLeft(s.copy(round = s.round + 1)) {
        (s, id) =>
          // units.get(id)
          // in each round, each unit that is still alive takes a turn,
          // resolving all of its actions before the next unit's turn begins
          s.units.get(id).filter(_.isAlive) match {
            case None => s
            case Some(self) =>
              s.turn(self)
          }
      }
    if(s.units == resultState.units) throw new IllegalStateException("Stuck")
    resultState
  }

  def runUntilBattleCompletes(s: State): State = {
    unfoldUntil(round)(_.isBattleCompleted)(s)
  }

  def playGame(gameInput: String): Int = {
    val lines = gameInput.split("\n")
    val mazeWithUnits = readMazeWithUnits(lines)
    val units = findAllUnits(mazeWithUnits).map(u => (u.id, u)).toMap
    val immutableMaze = readMaze(lines)
    val initialState = State(immutableMaze, units, 0)
    val finalState = runUntilBattleCompletes(initialState)
    finalState.showWithUnits()
    finalState.outcome
  }

  // 187800
  // 198354
  def answer1: Int = {
    playGame(inputTextFromResource)
  }

  def answer2: Int = 2

  def main(args: Array[String]): Unit = {
    println("answer1: " + answer1)
//    println("answer2: " + answer2)
  }
}
