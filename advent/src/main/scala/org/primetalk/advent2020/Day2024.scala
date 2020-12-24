package org.primetalk.advent2020

import org.primetalk.advent.tools.Geom2dUtils.{Direction, PosOps, Position}
import org.primetalk.advent.tools.{Display, Geom2dUtils, SequenceUtils, Utils}

import scala.annotation.tailrec
import scala.collection.View

/**
  * https://adventofcode.com/2020/day/24
  * {{{
  * --- Day 24: Lobby Layout ---
  *
  * Your raft makes it to the tropical island; it turns out that the small crab was an excellent navigator. You make your way to the resort.
  *
  * As you enter the lobby, you discover a small problem: the floor is being renovated. You can't even reach the check-in desk until they've finished installing the new tile floor.
  *
  * The tiles are all hexagonal; they need to be arranged in a hex grid with a very specific color pattern. Not in the mood to wait, you offer to help figure out the pattern.
  *
  * The tiles are all white on one side and black on the other. They start with the white side facing up. The lobby is large enough to fit whatever pattern might need to appear there.
  *
  * A member of the renovation crew gives you a list of the tiles that need to be flipped over (your puzzle input). Each line in the list identifies a single tile that needs to be flipped by giving a series of steps starting from a reference tile in the very center of the room. (Every line starts from the same reference tile.)
  *
  * Because the tiles are hexagonal, every tile has six neighbors: east, southeast, southwest, west, northwest, and northeast. These directions are given in your list, respectively, as e, se, sw, w, nw, and ne. A tile is identified by a series of these directions with no delimiters; for example, esenee identifies the tile you land on if you start at the reference tile and then move one tile east, one tile southeast, one tile northeast, and one tile east.
  *
  * Each time a tile is identified, it flips from white to black or from black to white. Tiles might be flipped more than once. For example, a line like esew flips a tile immediately adjacent to the reference tile, and a line like nwwswee flips the reference tile itself.
  *
  * Here is a larger example:
  *
  * sesenwnenenewseeswwswswwnenewsewsw
  * neeenesenwnwwswnenewnwwsewnenwseswesw
  * seswneswswsenwwnwse
  * nwnwneseeswswnenewneswwnewseswneseene
  * swweswneswnenwsewnwneneseenw
  * eesenwseswswnenwswnwnwsewwnwsene
  * sewnenenenesenwsewnenwwwse
  * wenwwweseeeweswwwnwwe
  * wsweesenenewnwwnwsenewsenwwsesesenwne
  * neeswseenwwswnwswswnw
  * nenwswwsewswnenenewsenwsenwnesesenew
  * enewnwewneswsewnwswenweswnenwsenwsw
  * sweneswneswneneenwnewenewwneswswnese
  * swwesenesewenwneswnwwneseswwne
  * enesenwswwswneneswsenwnewswseenwsese
  * wnwnesenesenenwwnenwsewesewsesesew
  * nenewswnwewswnenesenwnesewesw
  * eneswnwswnwsenenwnwnwwseeswneewsenese
  * neswnwewnwnwseenwseesewsenwsweewe
  * wseweeenwnesenwwwswnew
  *
  * In the above example, 10 tiles are flipped once (to black), and 5 more are flipped twice (to black, then back to white). After all of these instructions have been followed, a total of 10 tiles are black.
  *
  * Go through the renovation crew's list and determine which tiles they need to flip. After all of the instructions have been followed, how many tiles are left with the black side up?
  *
  * Your puzzle answer was 317.
  * --- Part Two ---
  *
  * The tile floor in the lobby is meant to be a living art exhibit. Every day, the tiles are all flipped according to the following rules:
  *
  *     Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
  *     Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
  *
  * Here, tiles immediately adjacent means the six tiles directly touching the tile in question.
  *
  * The rules are applied simultaneously to every tile; put another way, it is first determined which tiles need to be flipped, then they are all flipped at the same time.
  *
  * In the above example, the number of black tiles that are facing up after the given number of days has passed is as follows:
  *
  * Day 1: 15
  * Day 2: 12
  * Day 3: 25
  * Day 4: 14
  * Day 5: 23
  * Day 6: 28
  * Day 7: 41
  * Day 8: 37
  * Day 9: 49
  * Day 10: 37
  *
  * Day 20: 132
  * Day 30: 259
  * Day 40: 406
  * Day 50: 566
  * Day 60: 788
  * Day 70: 1106
  * Day 80: 1373
  * Day 90: 1844
  * Day 100: 2208
  *
  * After executing this process a total of 100 times, there would be 2208 black tiles facing up.
  *
  * How many tiles will be black after 100 days?
  *
  * Your puzzle answer was 3804.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  * }}}
  */
object Day2024 extends Utils {

  val input: IndexedSeq[String] = readResourceLines("day24.txt")
  type Dir6 = String

  type Path = List[Dir6]

  @tailrec
  def parseLine(line: String, acc: Path = Nil): Path = {
    if(line.isEmpty)
      acc.reverse
    else if(line.charAt(0) == 's' ||line.charAt(0) == 'n'){
      parseLine(line.substring(2), line.substring(0,2) :: acc)
    } else
      parseLine(line.substring(1), line.substring(0,1) :: acc)

  }
  val lines: IndexedSeq[Path] = input.map(parseLine(_))
  val dirMap = Map(
    "e" -> (2, 0),
    "w" -> (-2, 0),
    "se" -> (1, 1),
    "sw" -> (-1, 1),
    "ne" -> (1, -1),
    "nw" -> (-1, -1),
  )
  def dir(dir: Dir6): Direction =
    dirMap(dir)
  @tailrec
  def coord(path: Path, pos: Position = (0,0)): Position = path match {
    case Nil =>
      pos
    case ::(head, tail) =>
      coord(tail, pos + dir(head))
  }

  val allCoordinates: IndexedSeq[Position] = lines
    .map(coord(_))

  val blackCoordinates: Iterable[Position] = allCoordinates
    .groupBy(identity)
    .view.filter(_._2.size % 2 == 1)
    .keys

  //317
  lazy val answer1: Int =
    allCoordinates
      .groupBy(identity)
      .view.mapValues(_.size)
      .values
      .filter(_ % 2 == 1)
      .sum

  //Part 2
  val bounding: Geom2dUtils.Rectangle = Geom2dUtils.boundingRect(blackCoordinates.toSeq)

  val initial: Display[Char] = {
    val d = Display[Char](bounding.enlargeBy(102,102))
    d.fillAll(' ')
    blackCoordinates.foreach{ p => d(p) = '#' }
    d
  }
  val relPositions: List[Position] =
    dirMap.values.toList

  def step(d: Display[Char]): Display[Char] = {
    d.produceByRules(relPositions){
      case ('#', seq) =>
        val count = seq.count(_ == '#')
        if(count == 0 || count > 2)
          ' '
        else
          '#'
      case (' ', seq) =>
        val count = seq.count(_ == '#')
        if(count == 2)
          '#'
        else
          ' '
      case (c, _) => c
    }
  }

  def stat(d: Display[Char]): Int = {
    val values = d.points.filter(p => (p._1 + p._2) %2 == 0)
      .map(d.apply)
    values.count(_ == '#')
  }
  // ..., 3804
  lazy val answer2: Int = {
    val last = SequenceUtils.unfoldN(initial, 100)(step)
    stat(last)
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
