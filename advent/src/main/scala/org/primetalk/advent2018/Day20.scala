package org.primetalk.advent2018

import Geom2dUtils._
import org.primetalk.advent.tools.GraphUtils._
import org.primetalk.advent.tools.Utils

/**
  * --- Day 20: A Regular Map ---
  *
  * While you were learning about instruction pointers, the Elves made considerable progress. When you look up, you discover that the North Pole base construction project has completely surrounded you.
  *
  * The area you are in is made up entirely of rooms and doors. The rooms are arranged in a grid, and rooms only connect to adjacent rooms when a door is present between them.
  *
  * For example, drawing rooms as ., walls as #, doors as | or -, your current position as X, and where north is up, the area you're in might look like this:
  *
  * #####
  * #.|.#
  * #-###
  * #.|X#
  * #####
  *
  * You get the attention of a passing construction Elf and ask for a map. "I don't have time to draw out a map of this place - it's huge. Instead, I can give you directions to every room in the facility!" He writes down some directions on a piece of parchment and runs off. In the example above, the instructions might have been ^WNE$, a regular expression or "regex" (your puzzle input).
  *
  * The regex matches routes (like WNE for "west, north, east") that will take you from your current room through various doors in the facility. In aggregate, the routes will take you through every door in the facility at least once; mapping out all of these routes will let you build a proper map and find your way around.
  *
  * ^ and $ are at the beginning and end of your regex; these just mean that the regex doesn't match anything outside the routes it describes. (Specifically, ^ matches the start of the route, and $ matches the end of it.) These characters will not appear elsewhere in the regex.
  *
  * The rest of the regex matches various sequences of the characters N (north), S (south), E (east), and W (west). In the example above, ^WNE$ matches only one route, WNE, which means you can move west, then north, then east from your current position. Sequences of letters like this always match that exact route in the same order.
  *
  * Sometimes, the route can branch. A branch is given by a list of options separated by pipes (|) and wrapped in parentheses. So, ^N(E|W)N$ contains a branch: after going north, you must choose to go either east or west before finishing your route by going north again. By tracing out the possible routes after branching, you can determine where the doors are and, therefore, where the rooms are in the facility.
  *
  * For example, consider this regex: ^ENWWW(NEEE|SSE(EE|N))$
  *
  * This regex begins with ENWWW, which means that from your current position, all routes must begin by moving east, north, and then west three times, in that order. After this, there is a branch. Before you consider the branch, this is what you know about the map so far, with doors you aren't sure about marked with a ?:
  *
  * #?#?#?#?#
  * ?.|.|.|.?
  * #?#?#?#-#
  * ?X|.?
  * #?#?#
  *
  * After this point, there is (NEEE|SSE(EE|N)). This gives you exactly two options: NEEE and SSE(EE|N). By following NEEE, the map now looks like this:
  *
  * #?#?#?#?#
  * ?.|.|.|.?
  * #-#?#?#?#
  * ?.|.|.|.?
  * #?#?#?#-#
  * ?X|.?
  * #?#?#
  *
  * Now, only SSE(EE|N) remains. Because it is in the same parenthesized group as NEEE, it starts from the same room NEEE started in. It states that starting from that point, there exist doors which will allow you to move south twice, then east; this ends up at another branch. After that, you can either move east twice or north once. This information fills in the rest of the doors:
  *
  * #?#?#?#?#
  * ?.|.|.|.?
  * #-#?#?#?#
  * ?.|.|.|.?
  * #-#?#?#-#
  * ?.?.?X|.?
  * #-#-#?#?#
  * ?.|.|.|.?
  * #?#?#?#?#
  *
  * Once you've followed all possible routes, you know the remaining unknown parts are all walls, producing a finished map of the facility:
  *
  * #########
  * #.|.|.|.#
  * #-#######
  * #.|.|.|.#
  * #-#####-#
  * #.#.#X|.#
  * #-#-#####
  * #.|.|.|.#
  * #########
  *
  * Sometimes, a list of options can have an empty option, like (NEWS|WNSE|). This means that routes at this point could effectively skip the options in parentheses and move on immediately. For example, consider this regex and the corresponding map:
  *
  * ^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$
  *
  * ###########
  * #.|.#.|.#.#
  * #-###-#-#-#
  * #.|.|.#.#.#
  * #-#####-#-#
  * #.#.#X|.#.#
  * #-#-#####-#
  * #.#.|.|.|.#
  * #-###-###-#
  * #.|.|.#.|.#
  * ###########
  *
  * This regex has one main route which, at three locations, can optionally include additional detours and be valid: (NEWS|), (WNSE|), and (SWEN|). Regardless of which option is taken, the route continues from the position it is left at after taking those steps. So, for example, this regex matches all of the following routes (and more that aren't listed here):
  *
  * ENNWSWWSSSEENEENNN
  * ENNWSWWNEWSSSSEENEENNN
  * ENNWSWWNEWSSSSEENEESWENNNN
  * ENNWSWWSSSEENWNSEEENNN
  *
  * By following the various routes the regex matches, a full map of all of the doors and rooms in the facility can be assembled.
  *
  * To get a sense for the size of this facility, you'd like to determine which room is furthest from you: specifically, you would like to find the room for which the shortest path to that room would require passing through the most doors.
  *
  * In the first example (^WNE$), this would be the north-east corner 3 doors away.
  * In the second example (^ENWWW(NEEE|SSE(EE|N))$), this would be the south-east corner 10 doors away.
  * In the third example (^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$), this would be the north-east corner 18 doors away.
  *
  * Here are a few more examples:
  *
  * Regex: ^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$
  * Furthest room requires passing 23 doors
  *
  * #############
  * #.|.|.|.|.|.#
  * #-#####-###-#
  * #.#.|.#.#.#.#
  * #-#-###-#-#-#
  * #.#.#.|.#.|.#
  * #-#-#-#####-#
  * #.#.#.#X|.#.#
  * #-#-#-###-#-#
  * #.|.#.|.#.#.#
  * ###-#-###-#-#
  * #.|.#.|.|.#.#
  * #############
  *
  * Regex: ^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$
  * Furthest room requires passing 31 doors
  *
  * ###############
  * #.|.|.|.#.|.|.#
  * #-###-###-#-#-#
  * #.|.#.|.|.#.#.#
  * #-#########-#-#
  * #.#.|.|.|.|.#.#
  * #-#-#########-#
  * #.#.#.|X#.|.#.#
  * ###-#-###-#-#-#
  * #.|.#.#.|.#.|.#
  * #-###-#####-###
  * #.|.#.|.|.#.#.#
  * #-#-#####-#-#-#
  * #.#.|.|.|.#.|.#
  * ###############
  *
  * What is the largest number of doors you would be required to pass through to reach a room? That is, find the room for which the shortest path from your starting location to that room would require passing through the most doors; what is the fewest doors you can pass through to reach it?
  *
  * Your puzzle answer was 4155.
  * --- Part Two ---
  *
  * Okay, so the facility is big.
  *
  * How many rooms have a shortest path from your current location that pass through at least 1000 doors?
  *
  * Your puzzle answer was 8434.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day20ParseRegex {
  import fastparse._
  import NoWhitespace._
  sealed trait PathRegex
  case object EmptyPath extends PathRegex
  case class PlainPath(path: String) extends PathRegex
  case class Branch(p: Seq[PathRegex]) extends PathRegex
  case class Sequence(p: Seq[PathRegex]) extends PathRegex

  def dir[_ : P]: P[String] = P(CharIn("WNES").!)
  def emptyPath[_ : P]: P[EmptyPath.type] = P("").map(_ => EmptyPath)
  def plainPath[_ : P]: P[PlainPath] = P(CharIn("WNES").rep(1).!).map(PlainPath)
  def branch[_ : P]: P[Branch] = P(CharIn("(") ~ (expr|emptyPath).rep(min = 1, sep = "|")  ~ CharIn(")")).map(Branch)

  def expr[_ : P]: P[PathRegex] = P((plainPath|branch).rep(1)).map(seq => if(seq.size == 1) seq.head else Sequence(seq))

  def completeExpression[_ : P]: P[PathRegex] = P("^" ~ expr ~ "$")

  def parseRegex(s: String): PathRegex =
    parse(s, completeExpression(_)).get.value
}
object Day20 extends Utils {

  lazy val inputTextFromResource : String =
    readResourceAsString("day20.txt")

  val room = '.'
  val wall = '#'
  val door = Set('|', '-')
  val ourPosition = 'X'

  // W - west, N - north, E - east, S - south
  val directionMap = Map('W' -> Left, 'N' -> Up, 'E' -> Right, 'S' -> Down)


  import Day20ParseRegex._

  def parseRoute(line: String): Day20ParseRegex.PathRegex =
    Day20ParseRegex.parseRegex(line)

  def maxLength(p: Day20ParseRegex.PathRegex): Int = p match {
    case EmptyPath => 0
    case PlainPath(path) => path.length
    case Branch(branches) => branches.map(maxLength).max
    case Sequence(seq) => seq.map(maxLength).sum
  }

  def shortestPath(p: Day20ParseRegex.PathRegex): Int = p match {
    case EmptyPath => 0
    case PlainPath(path) => path.length
    case Branch(branches) => branches.map(shortestPath).min
    case Sequence(seq) => seq.map(shortestPath).sum
  }

  def shortestPathToFurthestRoom(p: Day20ParseRegex.PathRegex): Int = p match {
    case EmptyPath => 0
    case PlainPath(path) => path.length
    case Branch(branches) => branches.map(shortestPathToFurthestRoom).min
    case Sequence(seq) => seq.map(shortestPathToFurthestRoom).sum
  }

  def convertRegexPathToGraph(pathRegex: PathRegex, startPos: Position, graph: List[(Position, Position)] = Nil): (List[(Position, Position)], List[Position]) = pathRegex match {
    case EmptyPath =>
      (graph, List(startPos))
    case PlainPath(path) =>
      val directions = path.toCharArray.toSeq.map(directionMap).toList
      val (gr, np) = directions.foldLeft((graph, startPos)){
        case ((g, p), dir) =>
          val np = p + dir
          val edge = (p, np)
          (edge :: g, np)
      }
      (gr, List(np))
    case Branch(branches) =>
      val subgraphs: Seq[(List[(Position, Position)], List[Position])] = branches.map(convertRegexPathToGraph(_, startPos))
      val paths = subgraphs.map(_._1)
      val positions = subgraphs.flatMap(_._2).distinct.toList
      val gr = paths.foldLeft(graph)(_.reverse_:::(_))
      (gr, positions)
    case Sequence(seq) =>
      seq.foldLeft((graph, List(startPos))){
        case ((g, ps), subPath) =>
          val res: Seq[(List[(Position, Position)], List[Position])] = ps.map(convertRegexPathToGraph(subPath, _))
          val grs = res.map(_._1)
          val nps = res.flatMap(_._2).distinct.toList
          (grs.foldLeft(g)(_.reverse_:::(_)), nps)
      }
  }

  def shortestToFurthest(input: String): Int = {
    val r = parseRoute(input)
    val (edges, _) = convertRegexPathToGraph(r, origin)
    val gr = convertEdgesToUndirectedGraph(edges)
    val f = convertDependenciesToFunction(gr)
    val distances: Map[Position, (Int, ReversePath[Position])] = findShortestPathsForAllReachablePoints(f)(toVisit = Vector((origin, 0, Nil)), Map(origin -> (0, Nil)))
    val maxDistance = distances.map(_._2._1).max
    maxDistance
  }

  def countRoomsFurtherThanN(input: String, n: Int): Int = {
    val r = parseRoute(input)
    val (edges, _) = convertRegexPathToGraph(r, origin)
    val gr = convertEdgesToUndirectedGraph(edges)
    val f = convertDependenciesToFunction(gr)
    val distances: Map[Position, (Int, ReversePath[Position])] = findShortestPathsForAllReachablePoints(f)(toVisit = Vector((origin, 0, Nil)), Map(origin -> (0, Nil)))
    val distancesFurther = distances.filter(_._2._1 >= n)
    distancesFurther.size
  }
  // 4415
  // 16
  // 4151
  // 4155 // undirected graph
  lazy val answer1: Long = {
    shortestToFurthest(inputTextFromResource)
  }

  // Part 2
  // 8434
  lazy val answer2: Long = {
    countRoomsFurtherThanN(inputTextFromResource, 1000)
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}

