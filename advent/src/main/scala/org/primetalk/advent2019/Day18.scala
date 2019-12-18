package org.primetalk.advent2019

import org.primetalk.advent.tools.Geom2dUtils.Position
import org.primetalk.advent.tools.GraphUtils.PathInfo
import org.primetalk.advent.tools.{Display, GraphUtils, Utils}

/**
  *
  * https://adventofcode.com/2019/day/18
  *
  * --- Day 18: Many-Worlds Interpretation ---
  *
  * As you approach Neptune, a planetary security system detects you and activates a giant tractor beam on Triton! You have no choice but to land.
  *
  * A scan of the local area reveals only one interesting feature: a massive underground vault. You generate a map of the tunnels (your puzzle input). The tunnels are too narrow to move diagonally.
  *
  * Only one entrance (marked @) is present among the open passages (marked .) and stone walls (#), but you also detect an assortment of keys (shown as lowercase letters) and doors (shown as uppercase letters). Keys of a given letter open the door of the same letter: a opens A, b opens B, and so on. You aren't sure which key you need to disable the tractor beam, so you'll need to collect all of them.
  *
  * For example, suppose you have the following map:
  *
  * #########
  * #b.A.@.a#
  * #########
  *
  * Starting from the entrance (@), you can only access a large door (A) and a key (a). Moving toward the door doesn't help you, but you can move 2 steps to collect the key, unlocking A in the process:
  *
  * #########
  * #b.....@#
  * #########
  *
  * Then, you can move 6 steps to collect the only other key, b:
  *
  * #########
  * #@......#
  * #########
  *
  * So, collecting every key took a total of 8 steps.
  *
  * Here is a larger example:
  *
  * ########################
  * #f.D.E.e.C.b.A.@.a.B.c.#
  * ######################.#
  * #d.....................#
  * ########################
  *
  * The only reasonable move is to take key a and unlock door A:
  *
  * ########################
  * #f.D.E.e.C.b.....@.B.c.#
  * ######################.#
  * #d.....................#
  * ########################
  *
  * Then, do the same with key b:
  *
  * ########################
  * #f.D.E.e.C.@.........c.#
  * ######################.#
  * #d.....................#
  * ########################
  *
  * ...and the same with key c:
  *
  * ########################
  * #f.D.E.e.............@.#
  * ######################.#
  * #d.....................#
  * ########################
  *
  * Now, you have a choice between keys d and e. While key e is closer, collecting it now would be slower in the long run than collecting key d first, so that's the best choice:
  *
  * ########################
  * #f...E.e...............#
  * ######################.#
  * #@.....................#
  * ########################
  *
  * Finally, collect key e to unlock door E, then collect key f, taking a grand total of 86 steps.
  *
  * Here are a few more examples:
  *
  * ########################
  * #...............b.C.D.f#
  * #.######################
  * #.....@.a.B.c.d.A.e.F.g#
  * ########################
  *
  * Shortest path is 132 steps: b, a, c, d, f, e, g
  *
  * #################
  * #i.G..c...e..H.p#
  * ########.########
  * #j.A..b...f..D.o#
  * ########@########
  * #k.E..a...g..B.n#
  * ########.########
  * #l.F..d...h..C.m#
  * #################
  *
  * Shortest paths are 136 steps;
  * one is: a, f, b, j, g, n, h, d, l, o, e, p, c, i, k, m
  *
  * ########################
  * #@..............ac.GI.b#
  * ###d#e#f################
  * ###A#B#C################
  * ###g#h#i################
  * ########################
  *
  * Shortest paths are 81 steps; one is: a, c, f, i, d, g, b, e, h
  *
  * How many steps is the shortest path that collects all of the keys?
  *
  * Your puzzle answer was 4420.
  * --- Part Two ---
  *
  * You arrive at the vault only to discover that there is not one vault, but four - each with its own entrance.
  *
  * On your map, find the area in the middle that looks like this:
  *
  * ...
  * .@.
  * ...
  *
  * Update your map to instead use the correct data:
  *
  * @#@
  * ###
  * @#@
  *
  * This change will split your map into four separate sections, each with its own entrance:
  *
  * #######       #######
  * #a.#Cd#       #a.#Cd#
  * ##...##       ##@#@##
  * ##.@.##  -->  #######
  * ##...##       ##@#@##
  * #cB#Ab#       #cB#Ab#
  * #######       #######
  *
  * Because some of the keys are for doors in other vaults, it would take much too long to collect all of the keys by yourself. Instead, you deploy four remote-controlled robots. Each starts at one of the entrances (@).
  *
  * Your goal is still to collect all of the keys in the fewest steps, but now, each robot has its own position and can move independently. You can only remotely control a single robot at a time. Collecting a key instantly unlocks any corresponding doors, regardless of the vault in which the key or door is found.
  *
  * For example, in the map above, the top-left robot first collects key a, unlocking door A in the bottom-right vault:
  *
  * #######
  * #@.#Cd#
  * ##.#@##
  * #######
  * ##@#@##
  * #cB#.b#
  * #######
  *
  * Then, the bottom-right robot collects key b, unlocking door B in the bottom-left vault:
  *
  * #######
  * #@.#Cd#
  * ##.#@##
  * #######
  * ##@#.##
  * #c.#.@#
  * #######
  *
  * Then, the bottom-left robot collects key c:
  *
  * #######
  * #@.#.d#
  * ##.#@##
  * #######
  * ##.#.##
  * #@.#.@#
  * #######
  *
  * Finally, the top-right robot collects key d:
  *
  * #######
  * #@.#.@#
  * ##.#.##
  * #######
  * ##.#.##
  * #@.#.@#
  * #######
  *
  * In this example, it only took 8 steps to collect all of the keys.
  *
  * Sometimes, multiple robots might have keys available, or a robot might have to wait for multiple keys to be collected:
  *
  * ###############
  * #d.ABC.#.....a#
  * ######@#@######
  * ###############
  * ######@#@######
  * #b.....#.....c#
  * ###############
  *
  * First, the top-right, bottom-left, and bottom-right robots take turns collecting keys a, b, and c, a total of 6 + 6 + 6 = 18 steps. Then, the top-left robot can access key d, spending another 6 steps; collecting all of the keys here takes a minimum of 24 steps.
  *
  * Here's a more complex example:
  *
  * #############
  * #DcBa.#.GhKl#
  * #.###@#@#I###
  * #e#d#####j#k#
  * ###C#@#@###J#
  * #fEbA.#.FgHi#
  * #############
  *
  * Top-left robot collects key a.
  * Bottom-left robot collects key b.
  * Top-left robot collects key c.
  * Bottom-left robot collects key d.
  * Top-left robot collects key e.
  * Bottom-left robot collects key f.
  * Bottom-right robot collects key g.
  * Top-right robot collects key h.
  * Bottom-right robot collects key i.
  * Top-right robot collects key j.
  * Bottom-right robot collects key k.
  * Top-right robot collects key l.
  *
  * In the above example, the fewest steps to collect all of the keys is 32.
  *
  * Here's an example with more choices:
  *
  * #############
  * #g#f.D#..h#l#
  * #F###e#E###.#
  * #dCba@#@BcIJ#
  * #############
  * #nK.L@#@G...#
  * #M###N#H###.#
  * #o#m..#i#jk.#
  * #############
  *
  * One solution with the fewest steps is:
  *
  * Top-left robot collects key e.
  * Top-right robot collects key h.
  * Bottom-right robot collects key i.
  * Top-left robot collects key a.
  * Top-left robot collects key b.
  * Top-right robot collects key c.
  * Top-left robot collects key d.
  * Top-left robot collects key f.
  * Top-left robot collects key g.
  * Bottom-right robot collects key k.
  * Bottom-right robot collects key j.
  * Top-right robot collects key l.
  * Bottom-left robot collects key n.
  * Bottom-left robot collects key m.
  * Bottom-left robot collects key o.
  *
  * This example requires at least 72 steps to collect all keys.
  *
  * After updating your map and using the remote-controlled robots, what is the fewest steps necessary to collect all of the keys?
  *
  * Your puzzle answer was 2128.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day18 extends Utils {

  lazy val lines : Seq[String] =
    readResource("day18.txt").toSeq

  def display(lines: Seq[String]): Display[Char] = Display.readCharDisplay(lines)

  def allKeys(lines: Seq[String]): Set[Char] = lines.flatMap(l => l.toCharArray.filter(ch => ch.isLetter && ch.isLower)).toSet

  case class SearchState(position: Position, keys: Set[Char])

  lazy val answer1: Int = {
    val keys: Set[Char] = allKeys(lines)
    val d = display(lines)
    val startPositions = d.points.filter(p => d(p) == '@')
    val graph: SearchState => Seq[SearchState] = {
      case SearchState(p, ks) =>
        d.adjacentPositions(p).flatMap{ pp =>
          val l = d(pp)
          l match {
            case '.'|'@' =>
              Seq(SearchState(pp, ks))
            case letter if letter.isLower =>
              Seq(SearchState(pp, ks + letter))
            case letter if letter.isUpper =>
              if(ks.contains(letter.toLower))
                Seq(SearchState(pp, ks))
              else
                Seq()
            case '#' =>
              Seq()
            case unknown => throw new IllegalStateException(s"Unknown char at $p: $unknown")
          }
        }
    }
    val isFinish: SearchState => Boolean = {
      case SearchState(_, ks) => keys == ks
    }
    val shortest = GraphUtils.findAllShortestPaths2[SearchState](graph,isFinish)(
      startPositions.toVector.map{ p => (SearchState(p, Set()), PathInfo[SearchState](0, Nil))},
      Map(), Nil
    )
    shortest._1
  }

  // Part 2

  lazy val answer2: Int = {
    val keys: Set[Char] = allKeys(lines)
    val d = display(lines)
    val startPositions0 = d.points.filter(p => d(p) == '@')
    startPositions0.foreach{ s =>
      import org.primetalk.advent.tools.Geom2dUtils._
      d(s) = '#'
      mainDirections.foreach { dir =>
        d(s + dir) = '#'
      }
      directions8.filterNot(mainDirections.contains).foreach{ dir =>
        d(s + dir) = '@'
      }
    }
    println(d.showDisplay()())
    case class S1(ks: Set[Char], activeDroid: Int)

    type S2 = List[Position]
    val startPositions = d.points.filter(p => d(p) == '@').toList
    val droids: List[Int] = startPositions.indices.toList
    val graph: ((S1, S2)) => List[(S1, S2)] = {
      case (S1(ks, activeDroid), positions) =>

        val p = positions(activeDroid)
        d.adjacentPositions(p).toList.flatMap { pp =>
          val l = d(pp)
          l match {
            case '.' | '@' =>
              List((S1(ks, activeDroid), positions.updated(activeDroid, pp)))
            case letter if letter.isLower =>
              val nextKs = ks + letter
              // when we find a new key, we can continue moving the other droids
              droids.map { droid =>
                (S1(nextKs, droid), positions.updated(activeDroid, pp))
              }
            case letter if letter.isUpper =>
              if (ks.contains(letter.toLower))
                List((S1(ks, activeDroid), positions.updated(activeDroid, pp)))
              else
                Nil
            case '#' =>
              Nil
            case unknown => throw new IllegalStateException(s"Unknown char at $p: $unknown")
          }
        }

    }
    val isFinish: (S1, S2) => Boolean = {
      case (S1(ks,_), _) => keys == ks
    }
    val shortest = GraphUtils.findAllShortestPaths6[S1, S2](graph,isFinish)(
      droids.
        map(activeDroid => (S1(Set[Char](), activeDroid), startPositions, PathInfo[(S1, S2)](0, Nil))),
      Map(), S1(Set(), 0), Nil, Map()
    )
    shortest._1
  }



  def main(args: Array[String]): Unit = {
//    println("Answer1: " + answer1) // 2147483647 // 4420
    println("Answer2: " + answer2)
  }
}
