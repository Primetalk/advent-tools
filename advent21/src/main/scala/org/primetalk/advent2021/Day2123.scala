package org.primetalk.advent2021

import org.primetalk.advent3.tools.Utils
import org.primetalk.advent3.tools.GraphUtils._
import org.primetalk.advent3.tools.GraphUtils
import org.primetalk.advent3.tools.Priority
import org.primetalk.advent3.tools.MyPriorityQueue
import cats.kernel.Order
import cats.collections.Heap

/**
  * https://adventofcode.com/2021/day/23
  * --- Day 23: Amphipod ---
  * 
  * A group of amphipods notice your fancy submarine and flag you down. "With such an impressive shell," one amphipod says, "surely you can help us with a question that has stumped our best scientists."
  * 
  * They go on to explain that a group of timid, stubborn amphipods live in a nearby burrow. Four types of amphipods live there: Amber (A), Bronze (B), Copper (C), and Desert (D). They live in a burrow that consists of a hallway and four side rooms. The side rooms are initially full of amphipods, and the hallway is initially empty.
  * 
  * They give you a diagram of the situation (your puzzle input), including locations of each amphipod (A, B, C, or D, each of which is occupying an otherwise open space), walls (#), and open space (.).
  * 
  * For example:
  * 
  * #############
  * #...........#
  * ###B#C#B#D###
  *   #A#D#C#A#
  *   #########
  * 
  * The amphipods would like a method to organize every amphipod into side rooms so that each side room contains one type of amphipod and the types are sorted A-D going left to right, like this:
  * 
  * #############
  * #...........#
  * ###A#B#C#D###
  *   #A#B#C#D#
  *   #########
  * 
  * Amphipods can move up, down, left, or right so long as they are moving into an unoccupied open space. Each type of amphipod requires a different amount of energy to move one step: Amber amphipods require 1 energy per step, Bronze amphipods require 10 energy, Copper amphipods require 100, and Desert ones require 1000. The amphipods would like you to find a way to organize the amphipods that requires the least total energy.
  * 
  * However, because they are timid and stubborn, the amphipods have some extra rules:
  * 
  *     Amphipods will never stop on the space immediately outside any room. They can move into that space so long as they immediately continue moving. (Specifically, this refers to the four open spaces in the hallway that are directly above an amphipod starting position.)
  *     Amphipods will never move from the hallway into a room unless that room is their destination room and that room contains no amphipods which do not also have that room as their own destination. If an amphipod's starting room is not its destination room, it can stay in that room until it leaves the room. (For example, an Amber amphipod will not move from the hallway into the right three rooms, and will only move into the leftmost room if that room is empty or if it only contains other Amber amphipods.)
  *     Once an amphipod stops moving in the hallway, it will stay in that spot until it can move into a room. (That is, once any amphipod starts moving, any other amphipods currently in the hallway are locked in place and will not move again until they can move fully into a room.)
  * 
  * In the above example, the amphipods can be organized using a minimum of 12521 energy. One way to do this is shown below.
  * 
  * Starting configuration:
  * 
  * #############
  * #...........#
  * ###B#C#B#D###
  *   #A#D#C#A#
  *   #########
  * 
  * One Bronze amphipod moves into the hallway, taking 4 steps and using 40 energy:
  * 
  * #############
  * #...B.......#
  * ###B#C#.#D###
  *   #A#D#C#A#
  *   #########
  * 
  * The only Copper amphipod not in its side room moves there, taking 4 steps and using 400 energy:
  * 
  * #############
  * #...B.......#
  * ###B#.#C#D###
  *   #A#D#C#A#
  *   #########
  * 
  * A Desert amphipod moves out of the way, taking 3 steps and using 3000 energy, and then the Bronze amphipod takes its place, taking 3 steps and using 30 energy:
  * 
  * #############
  * #.....D.....#
  * ###B#.#C#D###
  *   #A#B#C#A#
  *   #########
  * 
  * The leftmost Bronze amphipod moves to its room using 40 energy:
  * 
  * #############
  * #.....D.....#
  * ###.#B#C#D###
  *   #A#B#C#A#
  *   #########
  * 
  * Both amphipods in the rightmost room move into the hallway, using 2003 energy in total:
  * 
  * #############
  * #.....D.D.A.#
  * ###.#B#C#.###
  *   #A#B#C#.#
  *   #########
  * 
  * Both Desert amphipods move into the rightmost room using 7000 energy:
  * 
  * #############
  * #.........A.#
  * ###.#B#C#D###
  *   #A#B#C#D#
  *   #########
  * 
  * Finally, the last Amber amphipod moves into its room, using 8 energy:
  * 
  * #############
  * #...........#
  * ###A#B#C#D###
  *   #A#B#C#D#
  *   #########
  * 
  * What is the least energy required to organize the amphipods?
  * 
  * Your puzzle answer was 15111.
  * --- Part Two ---
  * 
  * As you prepare to give the amphipods your solution, you notice that the diagram they handed you was actually folded up. As you unfold it, you discover an extra part of the diagram.
  * 
  * Between the first and second lines of text that contain amphipod starting positions, insert the following lines:
  * 
  *   #D#C#B#A#
  *   #D#B#A#C#
  * 
  * So, the above example now becomes:
  * 
  * #############
  * #...........#
  * ###B#C#B#D###
  *   #D#C#B#A#
  *   #D#B#A#C#
  *   #A#D#C#A#
  *   #########
  * 
  * The amphipods still want to be organized into rooms similar to before:
  * 
  * #############
  * #...........#
  * ###A#B#C#D###
  *   #A#B#C#D#
  *   #A#B#C#D#
  *   #A#B#C#D#
  *   #########
  * 
  * In this updated example, the least energy required to organize these amphipods is 44169:
  * 
  * #############
  * #...........#
  * ###B#C#B#D###
  *   #D#C#B#A#
  *   #D#B#A#C#
  *   #A#D#C#A#
  *   #########
  * 
  * #############
  * #..........D#
  * ###B#C#B#.###
  *   #D#C#B#A#
  *   #D#B#A#C#
  *   #A#D#C#A#
  *   #########
  * 
  * #############
  * #A.........D#
  * ###B#C#B#.###
  *   #D#C#B#.#
  *   #D#B#A#C#
  *   #A#D#C#A#
  *   #########
  * 
  * #############
  * #A........BD#
  * ###B#C#.#.###
  *   #D#C#B#.#
  *   #D#B#A#C#
  *   #A#D#C#A#
  *   #########
  * 
  * #############
  * #A......B.BD#
  * ###B#C#.#.###
  *   #D#C#.#.#
  *   #D#B#A#C#
  *   #A#D#C#A#
  *   #########
  * 
  * #############
  * #AA.....B.BD#
  * ###B#C#.#.###
  *   #D#C#.#.#
  *   #D#B#.#C#
  *   #A#D#C#A#
  *   #########
  * 
  * #############
  * #AA.....B.BD#
  * ###B#.#.#.###
  *   #D#C#.#.#
  *   #D#B#C#C#
  *   #A#D#C#A#
  *   #########
  * 
  * #############
  * #AA.....B.BD#
  * ###B#.#.#.###
  *   #D#.#C#.#
  *   #D#B#C#C#
  *   #A#D#C#A#
  *   #########
  * 
  * #############
  * #AA...B.B.BD#
  * ###B#.#.#.###
  *   #D#.#C#.#
  *   #D#.#C#C#
  *   #A#D#C#A#
  *   #########
  * 
  * #############
  * #AA.D.B.B.BD#
  * ###B#.#.#.###
  *   #D#.#C#.#
  *   #D#.#C#C#
  *   #A#.#C#A#
  *   #########
  * 
  * #############
  * #AA.D...B.BD#
  * ###B#.#.#.###
  *   #D#.#C#.#
  *   #D#.#C#C#
  *   #A#B#C#A#
  *   #########
  * 
  * #############
  * #AA.D.....BD#
  * ###B#.#.#.###
  *   #D#.#C#.#
  *   #D#B#C#C#
  *   #A#B#C#A#
  *   #########
  * 
  * #############
  * #AA.D......D#
  * ###B#.#.#.###
  *   #D#B#C#.#
  *   #D#B#C#C#
  *   #A#B#C#A#
  *   #########
  * 
  * #############
  * #AA.D......D#
  * ###B#.#C#.###
  *   #D#B#C#.#
  *   #D#B#C#.#
  *   #A#B#C#A#
  *   #########
  * 
  * #############
  * #AA.D.....AD#
  * ###B#.#C#.###
  *   #D#B#C#.#
  *   #D#B#C#.#
  *   #A#B#C#.#
  *   #########
  * 
  * #############
  * #AA.......AD#
  * ###B#.#C#.###
  *   #D#B#C#.#
  *   #D#B#C#.#
  *   #A#B#C#D#
  *   #########
  * 
  * #############
  * #AA.......AD#
  * ###.#B#C#.###
  *   #D#B#C#.#
  *   #D#B#C#.#
  *   #A#B#C#D#
  *   #########
  * 
  * #############
  * #AA.......AD#
  * ###.#B#C#.###
  *   #.#B#C#.#
  *   #D#B#C#D#
  *   #A#B#C#D#
  *   #########
  * 
  * #############
  * #AA.D.....AD#
  * ###.#B#C#.###
  *   #.#B#C#.#
  *   #.#B#C#D#
  *   #A#B#C#D#
  *   #########
  * 
  * #############
  * #A..D.....AD#
  * ###.#B#C#.###
  *   #.#B#C#.#
  *   #A#B#C#D#
  *   #A#B#C#D#
  *   #########
  * 
  * #############
  * #...D.....AD#
  * ###.#B#C#.###
  *   #A#B#C#.#
  *   #A#B#C#D#
  *   #A#B#C#D#
  *   #########
  * 
  * #############
  * #.........AD#
  * ###.#B#C#.###
  *   #A#B#C#D#
  *   #A#B#C#D#
  *   #A#B#C#D#
  *   #########
  * 
  * #############
  * #..........D#
  * ###A#B#C#.###
  *   #A#B#C#D#
  *   #A#B#C#D#
  *   #A#B#C#D#
  *   #########
  * 
  * #############
  * #...........#
  * ###A#B#C#D###
  *   #A#B#C#D#
  *   #A#B#C#D#
  *   #A#B#C#D#
  *   #########
  * 
  * Using the initial configuration from the full diagram, what is the least energy required to organize the amphipods?
  * 
  * Your puzzle answer was 47625.
  * 
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2123 extends Utils:

  opaque type AmphipodType = Char

  val A: AmphipodType = 'A'
  val B: AmphipodType = 'B'
  val C: AmphipodType = 'C'
  val D: AmphipodType = 'D'

  val energyPerStep = Map(A -> 1, B -> 10, C -> 100, D -> 1000)

  val amphipodTypes: IndexedSeq[AmphipodType] = IndexedSeq(A, B, C, D)

  case class Amphipod(amphipodType: AmphipodType, i: Int)

  class Game(val roomDepth: Int = 2):
    
    val amphipods =
      (for
        i <- 0 until roomDepth
        at <- amphipodTypes
      yield
        Amphipod(at, i)
        ).toList
  
    object Geometry:
      sealed trait Position
      case class Hallway(i: Int) extends Position
      case class InRoom(amphipodType: AmphipodType, depth: Int) extends Position

      type Room = IndexedSeq[InRoom] 

      val hallways: IndexedSeq[Hallway] = (0 to 10).map(Hallway.apply).toIndexedSeq

      def room(amphipodType: AmphipodType): Room = 
        for
          depth <- 0 until roomDepth
        yield
          InRoom(amphipodType, depth)

      val rooms: Map[AmphipodType, Room] = 
        amphipodTypes.map(amphipodType => 
          amphipodType -> room(amphipodType)
        ).toMap

      val amphipodRoomInHallwayPosition: Map[AmphipodType, Hallway] = 
        Map(
          A -> hallways(2),
          B -> hallways(4),
          C -> hallways(6),
          D -> hallways(8),
        )
    
      // val edges: GraphEdges[Position] =
      //   hallways.sliding(2)
      //       .map{ case IndexedSeq(a, b) => (a, b)}.toIndexedSeq ++
      //     rooms.values
      //       .flatMap(room => 
      //         room.sliding(2).map{ case IndexedSeq(a, b) => (a, b) }
      //       ).toIndexedSeq ++
      //     amphipodTypes
      //       .map(amphipodType => 
      //         (amphipodRoomInHallwayPosition(amphipodType), rooms(amphipodType)(0))
      //       )  

      // val g = edges.toUndirectedGraph

      // def adjacent(p: Position) = g(p)

      val nonStoppablePositions: Set[Position] = 
        Set(hallways(2), hallways(4), hallways(6), hallways(8))

      val canStop: Position => Boolean = 
        nonStoppablePositions.contains.andThen(!_)

      val stoppableHallway = hallways.filter(canStop).toList

      extension (amphipodType: AmphipodType)
        def canMoveInto(p: Position) = 
          p match
            case InRoom(a, _) if a != amphipodType => false
            case _ => true

      type DiagramOfSituation = Map[Position, Amphipod]

      extension (d: DiagramOfSituation)
        def isRoomBusyWithOtherAmphipods(amphipodType: AmphipodType): Boolean =
          room(amphipodType)
            .exists(inRoomPosition => d.get(inRoomPosition)
              .exists(_.amphipodType != amphipodType)
            )
        def findUnsafe(a: Amphipod): Position = 
          d.find(_._2 == a)
            .getOrElse(throw IllegalArgumentException(s"Couldn't find amphipod $a in $d"))._1
        def isCorrectlyBusy(ir: InRoom): Boolean =
          d.get(ir).exists(a => a.amphipodType == ir.amphipodType)

      def drawPath(from: Position, to: Position): IndexedSeq[Position] =
        if from == to then 
          IndexedSeq(from)
        else
          (from, to) match
            case (Hallway(i1), Hallway(i2)) => 
              (i1 to i2 by (math.signum(i2-i1))).map(Hallway.apply)
            case (h@Hallway(_), i@InRoom(at, _)) =>
              val attachPoint = amphipodRoomInHallwayPosition(at)
              drawPath(h, attachPoint) ++ 
                drawPath(rooms(at)(0), i)
            case (i@InRoom(at, _), h@Hallway(_)) => 
              val attachPoint = amphipodRoomInHallwayPosition(at)
              drawPath(i, rooms(at)(0)) ++ drawPath(attachPoint, h)
            case (InRoom(at1, i1), InRoom(at2, i2)) if at1 == at2 =>
              (i1 to i2 by (math.signum(i2-i1)))
                .map(i => rooms(at1)(i))
            case (InRoom(at, i1), i2@InRoom(_, _))  =>
              throw new IllegalArgumentException("There is no direct path between rooms of different types")

      def distance(p1: Position, p2: Position): Int =
        (p1, p2) match
          case (Hallway(i1), Hallway(i2)) => math.abs(i1 - i2)
          case (h@Hallway(i1), InRoom(at, i2)) => 
            val attachPoint = amphipodRoomInHallwayPosition(at)
            val d1 = distance(h, attachPoint)
            d1 + 1 + i2
          case (InRoom(at, i2), h@Hallway(i1)) => 
            val attachPoint = amphipodRoomInHallwayPosition(at)
            val d1 = distance(h, attachPoint)
            d1 + 1 + i2
          case (InRoom(at1, i1), InRoom(at2, i2)) if at1 == at2 =>
            math.abs(i1 - i2)
          case (InRoom(at, i1), i2@InRoom(_, _))  =>
            println("Searching distance between different rooms!")
            val attachPoint = amphipodRoomInHallwayPosition(at)
            distance(attachPoint, i2) + i1 + 1

    import Geometry._

    case class GameState(amphipodPositions: DiagramOfSituation, 
      energySpent: Long = 0L, countInPlace: Int = 0):

      def isFinal: Boolean =
        rooms.values.forall(room => room.forall(ir => amphipodPositions.isCorrectlyBusy(ir)))

      def moveAmphipodToRoom(a: Amphipod, pos: Hallway): List[GameState] =
        val at = a.amphipodType
        if amphipodPositions.isRoomBusyWithOtherAmphipods(at) then
          Nil
        else
          val boardWithoutA = amphipodPositions.removed(pos)
          val roomPositions = rooms(at).filterNot(boardWithoutA.contains)
          if roomPositions.isEmpty then 
            throw IllegalArgumentException(s"No room for $a in $amphipodPositions")
          else
            val targetPosition = roomPositions.maxBy(_.depth)
            val path = drawPath(pos, targetPosition).drop(1) // removing starting position
            if path.exists(boardWithoutA.contains) then 
              Nil
            else
              val energy = distance(pos, targetPosition) * energyPerStep(at)
              List(GameState(boardWithoutA.updated(targetPosition, a), energySpent + energy, countInPlace + 1))

      def moveAmphipodToHallway(a: Amphipod, pos: InRoom): List[GameState] =
        val at = a.amphipodType
        val boardWithoutA = amphipodPositions.removed(pos)
        stoppableHallway.flatMap{ potentialTarget => 
          val path = drawPath(pos, potentialTarget)
          if path.exists(boardWithoutA.contains) then 
            Nil
          else
            val energy = distance(pos, potentialTarget) * energyPerStep(at)
            List(GameState(boardWithoutA.updated(potentialTarget, a), energySpent + energy, countInPlace))
        }

      def moveAmphipod(a: Amphipod): List[GameState] = 
        val initialPosition = amphipodPositions.findUnsafe(a)
        
        initialPosition match
          case h@Hallway(_)   => moveAmphipodToRoom(a, h)
          case ir@InRoom(at, i) =>
            if at == a.amphipodType && (
                (i == roomDepth - 1) ||
                (i + 1 until roomDepth).forall(j =>
                  amphipodPositions.get(rooms(at)(j)).forall(_.amphipodType == at)
                )
              ) then
              List()// no need to move, because it's already in place.
            else if (0 until i).exists(j => amphipodPositions.get(rooms(at)(j)).isDefined) then
              List() // cannot move - 
            else
              moveAmphipodToHallway(a, ir)

      def step: List[GameState] =
        val res = amphipods.flatMap(moveAmphipod)
        val better = res.filter(_.countInPlace > countInPlace)
        if better.isEmpty then 
          res
        else
          better

      def charAt(p: Position): Char =
        amphipodPositions.get(p).map(_.amphipodType).getOrElse('.')

      def show: String = 
        s"############# $energySpent; $countInPlace\n" +
        "#" + hallways.map(charAt).mkString + "#\n" +
        "###" + amphipodTypes.map(at => charAt(rooms(at)(0))).mkString("#") + "###\n" +
        (1 until roomDepth).map(i => 
        "  #" + amphipodTypes.map(at => charAt(rooms(at)(i))).mkString("#") + "#  \n").mkString +
        "  #########  \n"
      def move(a: Amphipod, pos: Position): GameState =
        moveAmphipod(a)
          .find(g => g.amphipodPositions.findUnsafe(a) == pos)
          .getOrElse(throw IllegalArgumentException(s"Couldn't move $a to $pos in board:\n$show"))

    // #############
    // #...........#
    // ###B#C#B#D###
    //   #A#D#C#A#
    //   #########
    val exampleGameState = GameState(
      Map(
        Geometry.rooms(A)(0) -> Amphipod(B, 0),
        Geometry.rooms(A)(1) -> Amphipod(A, 0),
        Geometry.rooms(B)(0) -> Amphipod(C, 0),
        Geometry.rooms(B)(1) -> Amphipod(D, 0),
        Geometry.rooms(C)(0) -> Amphipod(B, 1),
        Geometry.rooms(C)(1) -> Amphipod(C, 1),
        Geometry.rooms(D)(0) -> Amphipod(D, 1),
        Geometry.rooms(D)(1) -> Amphipod(A, 1),
      )
    )
    
    // #############
    // #...........#
    // ###B#B#C#D###
    //   #D#C#A#A#
    //   #########
    val inputGameState = GameState(
      Map(
        rooms(A)(0) -> Amphipod(B, 0),
        rooms(A)(1) -> Amphipod(D, 0),
        rooms(B)(0) -> Amphipod(B, 1),
        rooms(B)(1) -> Amphipod(C, 0),
        rooms(C)(0) -> Amphipod(C, 1),
        rooms(C)(1) -> Amphipod(A, 0),
        rooms(D)(0) -> Amphipod(D, 1),
        rooms(D)(1) -> Amphipod(A, 1),
      )
    )
    val inputGameState2 = 
      inputGameState
        .move(Amphipod(D, 1), hallways(7))

    def findLowestEnergy(start: GameState): Long =
      given Order[GameState] = Order.by(gs => gs.energySpent - gs.countInPlace * 1000000L)
      var i: Int = 0 // tracking progress
      val found = GraphUtils.priorityFindAll2[GameState, GameState, Long](
        key => {
          i += 1
          if i % 1000000 == 0 then
            println(key.show)
          val nextPoints = key.step
          val (found, notYet) = nextPoints.partition(np => np.isFinal)
          PartialSearchResultWithPriority2( notYet, found)
        },
        (limit, found) =>
          if found.isEmpty then
            limit 
          else {
            val newLimit = math.min(found.map(_.energySpent).min, limit)
            if newLimit < limit then println(s"limit: $newLimit")
            newLimit
          },

        (limit,alternatives) => 
          if limit == Long.MaxValue then 
            (limit, alternatives)
          else
            (limit, alternatives.filter(_.energySpent <= limit))
      )(Long.MaxValue, Heap(start), Nil)
      val fnd = found.sortBy(_.energySpent)
      fnd.take(10).foreach(gs => println(gs.show))
      fnd.head.energySpent

  // 15131 15149 17109 15113 15111
  lazy val answer1: Long =
    object Game2 extends Game(2)
    import Game2._
    import Geometry._

    import GraphUtils._
    println(inputGameState2.show)
    Game2.findLowestEnergy(Game2.inputGameState2)

  //Part 2
  lazy val answer2: Long = 

    object Game4 extends Game(4)
    import Game4._
    import Geometry._

    // #D#C#B#A#
    // #D#B#A#C#
    def unfold(g: GameState): GameState =
      g.copy(amphipodPositions = 
        g.amphipodPositions.map{
          case (InRoom(at, 1), a) => (rooms(at)(3), a)
          case a => a
        } ++ 
        Map(
          rooms(A)(1) -> Amphipod(D, 2),
          rooms(A)(2) -> Amphipod(D, 3),
          rooms(B)(1) -> Amphipod(C, 2),
          rooms(B)(2) -> Amphipod(B, 2),
          rooms(C)(1) -> Amphipod(B, 3),
          rooms(C)(2) -> Amphipod(A, 2),
          rooms(D)(1) -> Amphipod(A, 3),
          rooms(D)(2) -> Amphipod(C, 3),
        )
        )
    val inputGameState4 = unfold(inputGameState)
    // val inputGameState4Hinted = 
    //   inputGameState4
        // .move(Amphipod(D, 1), hallways(10))
        // .move(Amphipod(A, 3), hallways(9))
    println(inputGameState4.show)
    // println(inputGameState4Hinted.show)
    Game4.findLowestEnergy(inputGameState4)

  def main(args: Array[String]): Unit =
    // without hints it's slow
    println("Answer1: " + answer1)
    println("Answer2: " + answer2) 
