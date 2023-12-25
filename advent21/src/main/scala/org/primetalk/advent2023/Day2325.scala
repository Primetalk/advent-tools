package org.primetalk.advent2023

import org.primetalk.advent3.tools.Utils
import org.primetalk.advent3.tools.GraphUtils.{*, given}

import scala.util.Random

/**
  * https://adventofcode.com/2023/day/25
--- Day 25: Snowverload ---

Still somehow without snow, you go to the last place you haven't checked: the center of Snow Island, directly below the waterfall.

Here, someone has clearly been trying to fix the problem. Scattered everywhere are hundreds of weather machines, almanacs, communication modules, hoof prints, machine parts, mirrors, lenses, and so on.

Somehow, everything has been wired together into a massive snow-producing apparatus, but nothing seems to be running. You check a tiny screen on one of the communication modules: Error 2023. It doesn't say what Error 2023 means, but it does have the phone number for a support line printed on it.

"Hi, you've reached Weather Machines And So On, Inc. How can I help you?" You explain the situation.

"Error 2023, you say? Why, that's a power overload error, of course! It means you have too many components plugged in. Try unplugging some components and--" You explain that there are hundreds of components here and you're in a bit of a hurry.

"Well, let's see how bad it is; do you see a big red reset button somewhere? It should be on its own module. If you push it, it probably won't fix anything, but it'll report how overloaded things are." After a minute or two, you find the reset button; it's so big that it takes two hands just to get enough leverage to push it. Its screen then displays:

SYSTEM OVERLOAD!

Connected components would require
power equal to at least 100 stars!

"Wait, how many components did you say are plugged in? With that much equipment, you could produce snow for an entire--" You disconnect the call.

You have nowhere near that many stars - you need to find a way to disconnect at least half of the equipment here, but it's already Christmas! You only have time to disconnect three wires.

Fortunately, someone left a wiring diagram (your puzzle input) that shows how the components are connected. For example:

jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr

Each line shows the name of a component, a colon, and then a list of other components to which that component is connected. Connections aren't directional; abc: xyz and xyz: abc both represent the same configuration. Each connection between two components is represented only once, so some components might only ever appear on the left or right side of a colon.

In this example, if you disconnect the wire between hfx/pzl, the wire between bvb/cmg, and the wire between nvd/jqt, you will divide the components into two separate, disconnected groups:

    9 components: cmg, frs, lhk, lsr, nvd, pzl, qnr, rsh, and rzs.
    6 components: bvb, hfx, jqt, ntq, rhn, and xhk.

Multiplying the sizes of these groups together produces 54.

Find the three wires you need to disconnect in order to divide the components into two separate groups. What do you get if you multiply the sizes of these two groups together?

Your puzzle answer was 518391.

The first half of this puzzle is complete! It provides one gold star: *
--- Part Two ---

You climb over weather machines, under giant springs, and narrowly avoid a pile of pipes as you find and disconnect the three wires.

A moment after you disconnect the last wire, the big red reset button module makes a small ding noise:

System overload resolved!
Power required is now 50 stars.

Out of the corner of your eye, you notice goggles and a loose-fitting hard hat peeking at you from behind an ultra crucible. You think you see a faint glow, but before you can investigate, you hear another small ding:

Power required is now 49 stars.

Please supply the necessary stars and
push the button to restart the system.

You don't have enough stars to restart the system, though. You need 6 more.
  */
object Day2325 extends Utils:

  val lines: IndexedSeq[String] = readThisObjectInputLines

  def parseLine(line: String): List[UEdge[String]] =
    val Array(from, tos) = line.split(':')
    val f = Set(from)
    val to = tos.trim.split(' ').toList
    to.map(f + _)

  val ugraph: UGraphEdges[String] =
    lines.flatMap(parseLine)

  /*
310: 518391
312: 518391
429: 518391
674: 518391
731: 518391
949: 518391
976: 518391
   */
  lazy val answer1: Int =
    val res = for
      i <- 310 until 311
      c = findARandomCut(ugraph, new Random(i))
      _ = if i%10 == 0 then println(s"$i: ${c.cut.size}")
      if c.cut.size == 3
      res = c.s1.size * c.s2.size
      //_ = println(s"$i: $res")
    yield
      res
    res.head

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
