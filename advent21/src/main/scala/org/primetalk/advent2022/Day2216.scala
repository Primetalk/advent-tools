package org.primetalk.advent2022

import org.primetalk.advent3.tools.Utils
import org.primetalk.advent3.tools.GraphUtils
import org.primetalk.advent3.tools.GraphUtils._
import cats.collections.Heap
import cats.kernel.Order

/**
  * https://adventofcode.com/2022/day/16
--- Day 16: Proboscidea Volcanium ---

The sensors have led you to the origin of the distress signal: yet another handheld device, just like the one the Elves gave you. However, you don't see any Elves around; instead, the device is surrounded by elephants! They must have gotten lost in these tunnels, and one of the elephants apparently figured out how to turn on the distress signal.

The ground rumbles again, much stronger this time. What kind of cave is this, exactly? You scan the cave with your handheld device; it reports mostly igneous rock, some ash, pockets of pressurized gas, magma... this isn't just a cave, it's a volcano!

You need to get the elephants out of here, quickly. Your device estimates that you have 30 minutes before the volcano erupts, so you don't have time to go back out the way you came in.

You scan the cave for other options and discover a network of pipes and pressure-release valves. You aren't sure how such a system got into a volcano, but you don't have time to complain; your device produces a report (your puzzle input) of each valve's flow rate if it were opened (in pressure per minute) and the tunnels you could use to move between the valves.

There's even a valve in the room you and the elephants are currently standing in labeled AA. You estimate it will take you one minute to open a single valve and one minute to follow any tunnel from one valve to another. What is the most pressure you could release?

For example, suppose you had the following scan output:

Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II

All of the valves begin closed. You start at valve AA, but it must be damaged or jammed or something: its flow rate is 0, so there's no point in opening it. However, you could spend one minute moving to valve BB and another minute opening it; doing so would release pressure during the remaining 28 minutes at a flow rate of 13, a total eventual pressure release of 28 * 13 = 364. Then, you could spend your third minute moving to valve CC and your fourth minute opening it, providing an additional 26 minutes of eventual pressure release at a flow rate of 2, or 52 total pressure released by valve CC.

Making your way through the tunnels like this, you could probably open many or all of the valves by the time 30 minutes have elapsed. However, you need to release as much pressure as possible, so you'll need to be methodical. Instead, consider this approach:

== Minute 1 ==
No valves are open.
You move to valve DD.

== Minute 2 ==
No valves are open.
You open valve DD.

== Minute 3 ==
Valve DD is open, releasing 20 pressure.
You move to valve CC.

== Minute 4 ==
Valve DD is open, releasing 20 pressure.
You move to valve BB.

== Minute 5 ==
Valve DD is open, releasing 20 pressure.
You open valve BB.

== Minute 6 ==
Valves BB and DD are open, releasing 33 pressure.
You move to valve AA.

== Minute 7 ==
Valves BB and DD are open, releasing 33 pressure.
You move to valve II.

== Minute 8 ==
Valves BB and DD are open, releasing 33 pressure.
You move to valve JJ.

== Minute 9 ==
Valves BB and DD are open, releasing 33 pressure.
You open valve JJ.

== Minute 10 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve II.

== Minute 11 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve AA.

== Minute 12 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve DD.

== Minute 13 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve EE.

== Minute 14 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve FF.

== Minute 15 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve GG.

== Minute 16 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve HH.

== Minute 17 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You open valve HH.

== Minute 18 ==
Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
You move to valve GG.

== Minute 19 ==
Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
You move to valve FF.

== Minute 20 ==
Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
You move to valve EE.

== Minute 21 ==
Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
You open valve EE.

== Minute 22 ==
Valves BB, DD, EE, HH, and JJ are open, releasing 79 pressure.
You move to valve DD.

== Minute 23 ==
Valves BB, DD, EE, HH, and JJ are open, releasing 79 pressure.
You move to valve CC.

== Minute 24 ==
Valves BB, DD, EE, HH, and JJ are open, releasing 79 pressure.
You open valve CC.

== Minute 25 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

== Minute 26 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

== Minute 27 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

== Minute 28 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

== Minute 29 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

== Minute 30 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

This approach lets you release the most pressure possible in 30 minutes with this valve layout, 1651.

Work out the steps to release the most pressure in 30 minutes. What is the most pressure you can release?

Your puzzle answer was 1638.
--- Part Two ---

You're worried that even with an optimal approach, the pressure released won't be enough. What if you got one of the elephants to help you?

It would take you 4 minutes to teach an elephant how to open the right valves in the right order, leaving you with only 26 minutes to actually execute your plan. Would having two of you working together be better, even if it means having less time? (Assume that you teach the elephant before opening any valves yourself, giving you both the same full 26 minutes.)

In the example above, you could teach the elephant to help you as follows:

== Minute 1 ==
No valves are open.
You move to valve II.
The elephant moves to valve DD.

== Minute 2 ==
No valves are open.
You move to valve JJ.
The elephant opens valve DD.

== Minute 3 ==
Valve DD is open, releasing 20 pressure.
You open valve JJ.
The elephant moves to valve EE.

== Minute 4 ==
Valves DD and JJ are open, releasing 41 pressure.
You move to valve II.
The elephant moves to valve FF.

== Minute 5 ==
Valves DD and JJ are open, releasing 41 pressure.
You move to valve AA.
The elephant moves to valve GG.

== Minute 6 ==
Valves DD and JJ are open, releasing 41 pressure.
You move to valve BB.
The elephant moves to valve HH.

== Minute 7 ==
Valves DD and JJ are open, releasing 41 pressure.
You open valve BB.
The elephant opens valve HH.

== Minute 8 ==
Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
You move to valve CC.
The elephant moves to valve GG.

== Minute 9 ==
Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
You open valve CC.
The elephant moves to valve FF.

== Minute 10 ==
Valves BB, CC, DD, HH, and JJ are open, releasing 78 pressure.
The elephant moves to valve EE.

== Minute 11 ==
Valves BB, CC, DD, HH, and JJ are open, releasing 78 pressure.
The elephant opens valve EE.

(At this point, all valves are open.)

== Minute 12 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

...

== Minute 20 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

...

== Minute 26 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

With the elephant helping, after 26 minutes, the best you could do would release a total of 1707 pressure.

With you and an elephant working together for 26 minutes, what is the most pressure you could release?

Your puzzle answer was 2400.

Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2216 extends Utils:

  val input = readThisObjectInput

  case class Room(name: String, rate: Int, tunnels: List[String])

  // 8:30
  def parseLine(line: String): Room =
    Room(
      name = line.substring(6,8),
      rate = parseFirstIntInString(line),
      tunnels = {
        val second = line.split(";")(1).trim
        val capitals = "[A-Z]+".r
        capitals.findAllIn(second).map(_.toString()).toList
      },
    )

  val rooms = readThisObjectInputLines.map(parseLine)

  val roomMap = rooms.map(r => r.name -> r).toMap

  val nonZeroRooms = rooms.filter(_.rate > 0).map(_.name).toSet

  val shortestPaths = ("AA" :: nonZeroRooms.toList).map(start =>
    start -> {
      val finish = nonZeroRooms - start
      object searcher extends ShortestPathAlgorithms[String]:
        val graphAsFunction: GraphAsFunction[String] = from => roomMap(from).tunnels

        val isFinish: String => Boolean = finish.contains

        val s = PathInfo(0, List(start))
      searcher.findAllShortestPaths8(List(searcher.s))
        .view.mapValues(_.length)
        .toList
        .filter(p => nonZeroRooms.contains(p ._1))
        .filterNot(_._1 == start)
    }
  ).toMap
  val maxTime = 30
  
  val distances = shortestPaths.view.mapValues(_.toMap).toMap
  val maxDistance = shortestPaths.flatMap(_._2.map(_._2)).max

  val mostDistantRooms = shortestPaths.flatMap((from, lst) => lst.filter(_._2==maxDistance).map((to, dist) => (from,to))).keys

  val List(room1, room2) = mostDistantRooms.toList

  val countGood = rooms.count(_.rate > 0)

  case class State(name: String, time: Int, opened: Set[String], rate: Int, total: Int):
    def next: List[State] =
      if time >= maxTime then 
        List()
      else if allOpened then
        List(this.copy(time = maxTime))
      else
        val room = roomMap(name)
        val opening = if opened.contains(name) || room.rate == 0 then 
          Nil
        else
          List(this.copy(
            time = if opened.size == countGood - 1 then 
                maxTime
              else
                time + 1, 
            opened = opened + name, 
            rate = rate + room.rate,
            total = total + room.rate * (maxTime - time - 1)))
        
        opening reverse_::: shortestPaths(name)
          .filterNot(p => opened.contains(p._1))
          .map((n, steps) => this.copy(name = n, time = time + steps))

    def allOpened = 
      opened.size == countGood
  val initialState = State("AA", 0, Set(), 0, 0)

  given stateOrder: Order[State] = Order.by(s => -(s.rate*10 - s.time*20))// - s.time*1))

  def generateAll(toVisit: Heap[State], size: Int = 0, result: Int = 0): Int =
    if size > 10000000 then 
      throw IllegalStateException(s"size=$size")
    toVisit.pop match
      case Some((head, tail)) => 
        if head.time >= maxTime then
          if head.total > result then 
            println(s"size = $size: $head")
          //head.total
          generateAll(tail, size - 1, math.max(head.total, result))
        else
          val next = head.next
          generateAll(tail ++ next, size + next.size, result)
      case None =>
        result
  // 1521 1638
  lazy val answer1: Int = 
    println(shortestPaths.toList.map((from, paths) => s"$from: ${paths.mkString(",")}").mkString("\n"))
    generateAll(Heap(initialState))//.map(_.flow).max

  //Part 2
  val maxTime2 = 26

  sealed trait Action
  case class Open(room: Room) extends Action
  case class Move(target: String, distance: Int) extends Action
  case object Step extends Action

  case class AgentState(
    at: String = "AA", 
    halfwayTo: Int = 0,// if agent is not yet in the room "at"
  ):
    def step = this.copy(halfwayTo = halfwayTo - 1)
    def act(action: Action): AgentState =
      action match
        case Open(room) =>
          AgentState(room.name, 0)
        case Move(target, distance) =>
          AgentState(target, distance -1)
        case Step =>
          step
      

  def openingTheSame(a1: Action, a2: Action): Boolean =
    (a1, a2) match
      case (Open(Room(a, _, _)), Open(Room(b, _, _))) if a == b => true
      //case (Move(a, _), Move(b, _)) if a == b => true
      case _ => false
    
  case class State2(time: Int = 0, 
    agents: Map[String, AgentState] = Map("me" -> AgentState(), "elephant" -> AgentState()),
    opened: Set[String] = Set(),
    rate: Int = 0,
    total: Int = 0,
    targetRate: Int = 0
  ):
    def next: List[State2] =
      if time >= maxTime2 || allOpened then 
        List()
      else
        val agentActions = agents.view.mapValues{
          case AgentState(at, halfwayTo) =>
            halfwayTo match 
              case 0 =>
                val currentRoom = roomMap(at)
                def moves = shortestPaths(at)
                    .filterNot(p => opened.contains(p._1))
                    .filterNot(p => p._2 + time + 1 >= maxTime)
                    .map(Move.apply)

                if opened.contains(at) || currentRoom.rate == 0 then 
                  moves
                else
                  Open(currentRoom) :: moves
              case _ => 
                List(Step)
        }
        val actionsMe = agentActions("me")
        val actionsElephant = agentActions("elephant")
        val (actionsMe2, actionsElephant2) = (actionsMe, actionsElephant) match
          case (Open(r1)::_, Open(r2)::tail2) if r1.name == r2.name =>
            (List(Open(r1)), tail2)
          case (Open(r1)::_, Open(r2)::_) =>
            (List(Open(r1)), List(Open(r2)))
          case (Open(r1)::_, actions2) =>
            (List(Open(r1)), actions2)
          case (actions1, Open(r2)::_) =>
            (actions1, List(Open(r2)))
          case _ =>
            (actionsMe, actionsElephant)
        for
          me <- actionsMe2
          elephant <- actionsElephant2
         // if !openingTheSame(me, elephant)
          actions = Map("me" -> me, "elephant" -> elephant)
          roomsOpening = actions.values
            .collect{ case Open(room) => room}
            .filterNot(r => opened.contains(r.name))
          deltaRate = roomsOpening.map(_.rate).sum
          deltaT = 1
            // (me, elephant) match 
            //   case (Move(at1, d1), Move(at2,d2)) =>
            //     math.min(d1, d2)
            //   case _ =>
            //     1
          
        yield
          State2(
            time = time + deltaT,
            agents = agents.map((name, st) => name -> st.act(actions(name))),
            opened = opened ++ roomsOpening.map(_.name),
            rate = rate + deltaRate,
            total = total + deltaRate * (maxTime2 - time - 1),
            targetRate = actions.values.collect{ case Move(at, distance) => roomMap(at).rate * (maxTime2 - time - 1 - distance)}.sum,
          )

    def allOpened = 
      opened.size == countGood

  given state2Order: Order[State2] = Order.reverse(
    Order.by(state2 => 
      distances(state2.agents("me").at).getOrElse(room1, 0) +
        distances(state2.agents("elephant").at).getOrElse(room2, 0)
      // state2.rate*10 - state2.time*state2.time*20
    )
  )

  def generateAll2(toVisit: Heap[State2], size: Int = 0, result: Int = 0): Int =
    if size % 1_000_000 == 0 then
      println(s"...size=$size")
    if size > 500_000_000 then 
      throw IllegalStateException(s"size=$size")
    toVisit.pop match
      case Some((head, tail)) => 
        if head.time >= maxTime2 || head.allOpened then
          if head.total > result then 
            println(s"size = $size: $head")
          //head.total
          generateAll2(tail, size - 1, math.max(head.total, result))
        else
          val next = head.next
          //generateAll2(tail ++ next, size + next.size, result)
          val (allopened, remaining) = next.partition(_.allOpened)
          val res2 = next.map(_.total).foldLeft(result)(math.max)
          if res2 > result then
            println(s"res2 = $res2; size = $size; tmin = ${next.map(_.time).min}")
          generateAll2(tail ++ remaining, size + next.size, res2)
      case None =>
        result

  // 1565 (102480 steps) 2087 (2436884 steps) 2131 45M steps 2274/ 2332 100M 2359
  lazy val answer2: Int = 
    println(shortestPaths.toList.map((from, paths) => s"$from: ${paths.mkString(",")}").mkString("\n"))
    println(mostDistantRooms.mkString(","))
    generateAll2(Heap(State2()))//.map(_.flow).max


  def main(args: Array[String]): Unit =
    //println("Answer1: " + answer1)
    println("Answer2: " + answer2)
