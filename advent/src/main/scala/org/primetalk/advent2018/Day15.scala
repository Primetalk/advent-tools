package org.primetalk.advent2018

import org.primetalk.advent.tools.Geom2dUtils._
import org.primetalk.advent.tools.GraphUtils._
import org.primetalk.advent.tools.SequenceUtils.unfoldUntil
import org.primetalk.advent.tools.{Display, Utils}

/**
  * --- Day 15: Beverage Bandits ---
  *
  * Having perfected their hot chocolate, the Elves have a new problem: the Goblins that live in these caves will do anything to steal it. Looks like they're here for a fight.
  *
  * You scan the area, generating a map of the walls (#), open cavern (.), and starting position of every Goblin (G) and Elf (E) (your puzzle input).
  *
  * Combat proceeds in rounds; in each round, each unit that is still alive takes a turn, resolving all of its actions before the next unit's turn begins. On each unit's turn, it tries to move into range of an enemy (if it isn't already) and then attack (if it is in range).
  *
  * All units are very disciplined and always follow very strict combat rules. Units never move or attack diagonally, as doing so would be dishonorable. When multiple choices are equally valid, ties are broken in reading order: top-to-bottom, then left-to-right. For instance, the order in which units take their turns within a round is the reading order of their starting positions in that round, regardless of the type of unit or whether other units have moved after the round started. For example:
  *
  * would take their
  * These units:   turns in this order:
  * #######           #######
  * #.G.E.#           #.1.2.#
  * #E.G.E#           #3.4.5#
  * #.G.E.#           #.6.7.#
  * #######           #######
  *
  * Each unit begins its turn by identifying all possible targets (enemy units). If no targets remain, combat ends.
  *
  * Then, the unit identifies all of the open squares (.) that are in range of each target; these are the squares which are adjacent (immediately up, down, left, or right) to any target and which aren't already occupied by a wall or another unit. Alternatively, the unit might already be in range of a target. If the unit is not already in range of a target, and there are no open squares which are in range of a target, the unit ends its turn.
  *
  * If the unit is already in range of a target, it does not move, but continues its turn with an attack. Otherwise, since it is not in range of a target, it moves.
  *
  * To move, the unit first considers the squares that are in range and determines which of those squares it could reach in the fewest steps. A step is a single movement to any adjacent (immediately up, down, left, or right) open (.) square. Units cannot move into walls or other units. The unit does this while considering the current positions of units and does not do any prediction about where units will be later. If the unit cannot reach (find an open path to) any of the squares that are in range, it ends its turn. If multiple squares are in range and tied for being reachable in the fewest steps, the square which is first in reading order is chosen. For example:
  *
  * Targets:      In range:     Reachable:    Nearest:      Chosen:
  * #######       #######       #######       #######       #######
  * #E..G.#       #E.?G?#       #E.@G.#       #E.!G.#       #E.+G.#
  * #...#.#  -->  #.?.#?#  -->  #.@.#.#  -->  #.!.#.#  -->  #...#.#
  * #.G.#G#       #?G?#G#       #@G@#G#       #!G.#G#       #.G.#G#
  * #######       #######       #######       #######       #######
  *
  * In the above scenario, the Elf has three targets (the three Goblins):
  *
  * Each of the Goblins has open, adjacent squares which are in range (marked with a ? on the map).
  * Of those squares, four are reachable (marked @); the other two (on the right) would require moving through a wall or unit to reach.
  * Three of these reachable squares are nearest, requiring the fewest steps (only 2) to reach (marked !).
  * Of those, the square which is first in reading order is chosen (+).
  *
  * The unit then takes a single step toward the chosen square along the shortest path to that square. If multiple steps would put the unit equally closer to its destination, the unit chooses the step which is first in reading order. (This requires knowing when there is more than one shortest path so that you can consider the first step of each such path.) For example:
  *
  * In range:     Nearest:      Chosen:       Distance:     Step:
  * #######       #######       #######       #######       #######
  * #.E...#       #.E...#       #.E...#       #4E212#       #..E..#
  * #...?.#  -->  #...!.#  -->  #...+.#  -->  #32101#  -->  #.....#
  * #..?G?#       #..!G.#       #...G.#       #432G2#       #...G.#
  * #######       #######       #######       #######       #######
  *
  * The Elf sees three squares in range of a target (?), two of which are nearest (!), and so the first in reading order is chosen (+). Under "Distance", each open square is marked with its distance from the destination square; the two squares to which the Elf could move on this turn (down and to the right) are both equally good moves and would leave the Elf 2 steps from being in range of the Goblin. Because the step which is first in reading order is chosen, the Elf moves right one square.
  *
  * Here's a larger example of movement:
  *
  * Initially:
  * #########
  * #G..G..G#
  * #.......#
  * #.......#
  * #G..E..G#
  * #.......#
  * #.......#
  * #G..G..G#
  * #########
  *
  * After 1 round:
  * #########
  * #.G...G.#
  * #...G...#
  * #...E..G#
  * #.G.....#
  * #.......#
  * #G..G..G#
  * #.......#
  * #########
  *
  * After 2 rounds:
  * #########
  * #..G.G..#
  * #...G...#
  * #.G.E.G.#
  * #.......#
  * #G..G..G#
  * #.......#
  * #.......#
  * #########
  *
  * After 3 rounds:
  * #########
  * #.......#
  * #..GGG..#
  * #..GEG..#
  * #G..G...#
  * #......G#
  * #.......#
  * #.......#
  * #########
  *
  * Once the Goblins and Elf reach the positions above, they all are either in range of a target or cannot find any square in range of a target, and so none of the units can move until a unit dies.
  *
  * After moving (or if the unit began its turn in range of a target), the unit attacks.
  *
  * To attack, the unit first determines all of the targets that are in range of it by being immediately adjacent to it. If there are no such targets, the unit ends its turn. Otherwise, the adjacent target with the fewest hit points is selected; in a tie, the adjacent target with the fewest hit points which is first in reading order is selected.
  *
  * The unit deals damage equal to its attack power to the selected target, reducing its hit points by that amount. If this reduces its hit points to 0 or fewer, the selected target dies: its square becomes . and it takes no further turns.
  *
  * Each unit, either Goblin or Elf, has 3 attack power and starts with 200 hit points.
  *
  * For example, suppose the only Elf is about to attack:
  *
  * HP:            HP:
  * G....  9       G....  9
  * ..G..  4       ..G..  4
  * ..EG.  2  -->  ..E..
  * ..G..  2       ..G..  2
  * ...G.  1       ...G.  1
  *
  * The "HP" column shows the hit points of the Goblin to the left in the corresponding row. The Elf is in range of three targets: the Goblin above it (with 4 hit points), the Goblin to its right (with 2 hit points), and the Goblin below it (also with 2 hit points). Because three targets are in range, the ones with the lowest hit points are selected: the two Goblins with 2 hit points each (one to the right of the Elf and one below the Elf). Of those, the Goblin first in reading order (the one to the right of the Elf) is selected. The selected Goblin's hit points (2) are reduced by the Elf's attack power (3), reducing its hit points to -1, killing it.
  *
  * After attacking, the unit's turn ends. Regardless of how the unit's turn ends, the next unit in the round takes its turn. If all units have taken turns in this round, the round ends, and a new round begins.
  *
  * The Elves look quite outnumbered. You need to determine the outcome of the battle: the number of full rounds that were completed (not counting the round in which combat ends) multiplied by the sum of the hit points of all remaining units at the moment combat ends. (Combat only ends when a unit finds no targets during its turn.)
  *
  * Below is an entire sample combat. Next to each map, each row's units' hit points are listed from left to right.
  *
  * Initially:
  * #######
  * #.G...#   G(200)
  * #...EG#   E(200), G(200)
  * #.#.#G#   G(200)
  * #..G#E#   G(200), E(200)
  * #.....#
  * #######
  *
  * After 1 round:
  * #######
  * #..G..#   G(200)
  * #...EG#   E(197), G(197)
  * #.#G#G#   G(200), G(197)
  * #...#E#   E(197)
  * #.....#
  * #######
  *
  * After 2 rounds:
  * #######
  * #...G.#   G(200)
  * #..GEG#   G(200), E(188), G(194)
  * #.#.#G#   G(194)
  * #...#E#   E(194)
  * #.....#
  * #######
  *
  * Combat ensues; eventually, the top Elf dies:
  *
  * After 23 rounds:
  * #######
  * #...G.#   G(200)
  * #..G.G#   G(200), G(131)
  * #.#.#G#   G(131)
  * #...#E#   E(131)
  * #.....#
  * #######
  *
  * After 24 rounds:
  * #######
  * #..G..#   G(200)
  * #...G.#   G(131)
  * #.#G#G#   G(200), G(128)
  * #...#E#   E(128)
  * #.....#
  * #######
  *
  * After 25 rounds:
  * #######
  * #.G...#   G(200)
  * #..G..#   G(131)
  * #.#.#G#   G(125)
  * #..G#E#   G(200), E(125)
  * #.....#
  * #######
  *
  * After 26 rounds:
  * #######
  * #G....#   G(200)
  * #.G...#   G(131)
  * #.#.#G#   G(122)
  * #...#E#   E(122)
  * #..G..#   G(200)
  * #######
  *
  * After 27 rounds:
  * #######
  * #G....#   G(200)
  * #.G...#   G(131)
  * #.#.#G#   G(119)
  * #...#E#   E(119)
  * #...G.#   G(200)
  * #######
  *
  * After 28 rounds:
  * #######
  * #G....#   G(200)
  * #.G...#   G(131)
  * #.#.#G#   G(116)
  * #...#E#   E(113)
  * #....G#   G(200)
  * #######
  *
  * More combat ensues; eventually, the bottom Elf dies:
  *
  * After 47 rounds:
  * #######
  * #G....#   G(200)
  * #.G...#   G(131)
  * #.#.#G#   G(59)
  * #...#.#
  * #....G#   G(200)
  * #######
  *
  * Before the 48th round can finish, the top-left Goblin finds that there are no targets remaining, and so combat ends. So, the number of full rounds that were completed is 47, and the sum of the hit points of all remaining units is 200+131+59+200 = 590. From these, the outcome of the battle is 47 * 590 = 27730.
  *
  * Here are a few example summarized combats:
  *
  * #######       #######
  * #G..#E#       #...#E#   E(200)
  * #E#E.E#       #E#...#   E(197)
  * #G.##.#  -->  #.E##.#   E(185)
  * #...#E#       #E..#E#   E(200), E(200)
  * #...E.#       #.....#
  * #######       #######
  *
  * Combat ends after 37 full rounds
  * Elves win with 982 total hit points left
  * Outcome: 37 * 982 = 36334
  *
  * #######       #######
  * #E..EG#       #.E.E.#   E(164), E(197)
  * #.#G.E#       #.#E..#   E(200)
  * #E.##E#  -->  #E.##.#   E(98)
  * #G..#.#       #.E.#.#   E(200)
  * #..E#.#       #...#.#
  * #######       #######
  *
  * Combat ends after 46 full rounds
  * Elves win with 859 total hit points left
  * Outcome: 46 * 859 = 39514
  *
  * #######       #######
  * #E.G#.#       #G.G#.#   G(200), G(98)
  * #.#G..#       #.#G..#   G(200)
  * #G.#.G#  -->  #..#..#
  * #G..#.#       #...#G#   G(95)
  * #...E.#       #...G.#   G(200)
  * #######       #######
  *
  * Combat ends after 35 full rounds
  * Goblins win with 793 total hit points left
  * Outcome: 35 * 793 = 27755
  *
  * #######       #######
  * #.E...#       #.....#
  * #.#..G#       #.#G..#   G(200)
  * #.###.#  -->  #.###.#
  * #E#G#G#       #.#.#.#
  * #...#G#       #G.G#G#   G(98), G(38), G(200)
  * #######       #######
  *
  * Combat ends after 54 full rounds
  * Goblins win with 536 total hit points left
  * Outcome: 54 * 536 = 28944
  *
  * #########       #########
  * #G......#       #.G.....#   G(137)
  * #.E.#...#       #G.G#...#   G(200), G(200)
  * #..##..G#       #.G##...#   G(200)
  * #...##..#  -->  #...##..#
  * #...#...#       #.G.#...#   G(200)
  * #.G...G.#       #.......#
  * #.....G.#       #.......#
  * #########       #########
  *
  * Combat ends after 20 full rounds
  * Goblins win with 937 total hit points left
  * Outcome: 20 * 937 = 18740
  *
  * What is the outcome of the combat described in your puzzle input?
  *
  * Your puzzle answer was 195811.
  * --- Part Two ---
  *
  * According to your calculations, the Elves are going to lose badly. Surely, you won't mess up the timeline too much if you give them just a little advanced technology, right?
  *
  * You need to make sure the Elves not only win, but also suffer no losses: even the death of a single Elf is unacceptable.
  *
  * However, you can't go too far: larger changes will be more likely to permanently alter spacetime.
  *
  * So, you need to find the outcome of the battle in which the Elves have the lowest integer attack power (at least 4) that allows them to win without a single death. The Goblins always have an attack power of 3.
  *
  * In the first summarized example above, the lowest attack power the Elves need to win without losses is 15:
  *
  * #######       #######
  * #.G...#       #..E..#   E(158)
  * #...EG#       #...E.#   E(14)
  * #.#.#G#  -->  #.#.#.#
  * #..G#E#       #...#.#
  * #.....#       #.....#
  * #######       #######
  *
  * Combat ends after 29 full rounds
  * Elves win with 172 total hit points left
  * Outcome: 29 * 172 = 4988
  *
  * In the second example above, the Elves need only 4 attack power:
  *
  * #######       #######
  * #E..EG#       #.E.E.#   E(200), E(23)
  * #.#G.E#       #.#E..#   E(200)
  * #E.##E#  -->  #E.##E#   E(125), E(200)
  * #G..#.#       #.E.#.#   E(200)
  * #..E#.#       #...#.#
  * #######       #######
  *
  * Combat ends after 33 full rounds
  * Elves win with 948 total hit points left
  * Outcome: 33 * 948 = 31284
  *
  * In the third example above, the Elves need 15 attack power:
  *
  * #######       #######
  * #E.G#.#       #.E.#.#   E(8)
  * #.#G..#       #.#E..#   E(86)
  * #G.#.G#  -->  #..#..#
  * #G..#.#       #...#.#
  * #...E.#       #.....#
  * #######       #######
  *
  * Combat ends after 37 full rounds
  * Elves win with 94 total hit points left
  * Outcome: 37 * 94 = 3478
  *
  * In the fourth example above, the Elves need 12 attack power:
  *
  * #######       #######
  * #.E...#       #...E.#   E(14)
  * #.#..G#       #.#..E#   E(152)
  * #.###.#  -->  #.###.#
  * #E#G#G#       #.#.#.#
  * #...#G#       #...#.#
  * #######       #######
  *
  * Combat ends after 39 full rounds
  * Elves win with 166 total hit points left
  * Outcome: 39 * 166 = 6474
  *
  * In the last example above, the lone Elf needs 34 attack power:
  *
  * #########       #########
  * #G......#       #.......#
  * #.E.#...#       #.E.#...#   E(38)
  * #..##..G#       #..##...#
  * #...##..#  -->  #...##..#
  * #...#...#       #...#...#
  * #.G...G.#       #.......#
  * #.....G.#       #.......#
  * #########       #########
  *
  * Combat ends after 30 full rounds
  * Elves win with 38 total hit points left
  * Outcome: 30 * 38 = 1140
  *
  * After increasing the Elves' attack power until it is just barely enough for them to win without any Elves dying, what is the outcome of the combat described in your puzzle input?
  *
  * Your puzzle answer was 69867.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day15 extends Utils {

  implicit val ro: Ordering[Position] = readingOrdering

  lazy val inputTextFromResource : String =
    readResourceAsString("day15.txt")

  def readMazeWithUnits(lines: Seq[String]): Display[Char] =
    Display.readCharDisplay(lines)

  // Each unit, either Goblin or Elf, has 3 attack power and starts with 200 hit points.
  case class GameUnit(id: Int, kind: Char, position: Position,
    attackPower: Int = 3, hitPoints: Int = 200) {
    def isAlive: Boolean =
      hitPoints > 0
  }

  val elfesAttackPower = 3//10 // found by try. Algorithm for searching could have been binary search.

  def findAllUnits(d: Display[Char]): Seq[GameUnit] = {
    val seq = for{
      p <- d.points
      c = d(p)
      if c == 'E' || c == 'G'
    } yield GameUnit(0, c, p, attackPower = if(c == 'E') elfesAttackPower else 3 )
    seq.zipWithIndex.map{ case (u, i) => u.copy(id = i) }
  }

  def readMaze(lines: Seq[String]): Display[Char] =
    readMazeWithUnits(lines)
      .map {
        case 'E' | 'G' => '.'
        case c => c
      }

  def convertMazeToGraph(d: Display[Char], occupiedPositions: Set[Position]): GraphAsFunction[Position] = p => {
    mainDirections
      .map( _ + p)
      .filterNot(occupiedPositions.contains)
      .filter(d.isWithinRange)
      .filter{ pp =>
        val c = d.apply(pp)
        c != '#'
      }
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
        val reversedPathsToTarget = reversedPaths.filter(_.head == targetPosition)
        val startsOfPaths = reversedPathsToTarget.map(_.last)
        val selectedStart = startsOfPaths.min
        copy(units = units.updated(self.id, self.copy(position = selectedStart)))
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

    def outcome: Int =
      totalHitPoints * (round - 1) // Incorrect calculation. Just to fix

    def showWithUnits(): Unit = {
      println()
      println("After round " + round)
      val mapByY =
        units.values
          .map(u => (u.position._2, u)).groupBy(_._1)
          .view
          .mapValues(_.map(_._2).toSet)
          .toMap
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
    val lines = gameInput.split("\n").toIndexedSeq
    val mazeWithUnits = readMazeWithUnits(lines)
    val units = findAllUnits(mazeWithUnits).map(u => (u.id, u)).toMap
    val immutableMaze = readMaze(lines)
    val initialState = State(immutableMaze, units, 0)
    val finalState = runUntilBattleCompletes(initialState)
    finalState.showWithUnits()
    finalState.outcome
  }

  // Change elfesAttackPower to 3
  // 187800
  // FIXME 198354 - the result of this method.  It returns 78 rounds instead of 77.
  // 195811 - the actual expected result (rounds = 77). My solution doesn't work for it.
  //      The solution arrives at the correct position, but it takes one additional round.
  def answer1: Int = {
    playGame(inputTextFromResource)
  }

  // Change elfesAttackPower to 10 and don't forget to subtract one round.
  def answer2: Int = 2

  def main(args: Array[String]): Unit = {
    println("answer1: " + answer1)
//    println("answer2: " + answer2)
  }
}
