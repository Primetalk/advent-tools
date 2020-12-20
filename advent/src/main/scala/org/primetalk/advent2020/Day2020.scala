package org.primetalk.advent2020

import org.primetalk.advent.tools.Geom2dUtils.{Direction, Left, Position, Up, PosOps}
import org.primetalk.advent.tools.{Display, Geom2dUtils, Utils}

import scala.annotation.tailrec
import scala.collection.MapView

/**
  * https://adventofcode.com/2020/day/20
  * --- Day 20: Jurassic Jigsaw ---
  *
  * The high-speed train leaves the forest and quickly carries you south. You can even see a desert in the distance! Since you have some spare time, you might as well see if there was anything interesting in the image the Mythical Information Bureau satellite captured.
  *
  * After decoding the satellite messages, you discover that the data actually contains many small images created by the satellite's camera array. The camera array consists of many cameras; rather than produce a single square image, they produce many smaller square image tiles that need to be reassembled back into a single image.
  *
  * Each camera in the camera array returns a single monochrome image tile with a random unique ID number. The tiles (your puzzle input) arrived in a random order.
  *
  * Worse yet, the camera array appears to be malfunctioning: each image tile has been rotated and flipped to a random orientation. Your first task is to reassemble the original image by orienting the tiles so they fit together.
  *
  * To show how the tiles should be reassembled, each tile's image data includes a border that should line up exactly with its adjacent tiles. All tiles have this border, and the border lines up exactly when the tiles are both oriented correctly. Tiles at the edge of the image also have this border, but the outermost edges won't line up with any other tiles.
  *
  * For example, suppose you have the following nine tiles:
  *
  * Tile 2311:
  * ..##.#..#.
  * ##..#.....
  * #...##..#.
  * ####.#...#
  * ##.##.###.
  * ##...#.###
  * .#.#.#..##
  * ..#....#..
  * ###...#.#.
  * ..###..###
  *
  * Tile 1951:
  * #.##...##.
  * #.####...#
  * .....#..##
  * #...######
  * .##.#....#
  * .###.#####
  * ###.##.##.
  * .###....#.
  * ..#.#..#.#
  * #...##.#..
  *
  * Tile 1171:
  * ####...##.
  * #..##.#..#
  * ##.#..#.#.
  * .###.####.
  * ..###.####
  * .##....##.
  * .#...####.
  * #.##.####.
  * ####..#...
  * .....##...
  *
  * Tile 1427:
  * ###.##.#..
  * .#..#.##..
  * .#.##.#..#
  * #.#.#.##.#
  * ....#...##
  * ...##..##.
  * ...#.#####
  * .#.####.#.
  * ..#..###.#
  * ..##.#..#.
  *
  * Tile 1489:
  * ##.#.#....
  * ..##...#..
  * .##..##...
  * ..#...#...
  * #####...#.
  * #..#.#.#.#
  * ...#.#.#..
  * ##.#...##.
  * ..##.##.##
  * ###.##.#..
  *
  * Tile 2473:
  * #....####.
  * #..#.##...
  * #.##..#...
  * ######.#.#
  * .#...#.#.#
  * .#########
  * .###.#..#.
  * ########.#
  * ##...##.#.
  * ..###.#.#.
  *
  * Tile 2971:
  * ..#.#....#
  * #...###...
  * #.#.###...
  * ##.##..#..
  * .#####..##
  * .#..####.#
  * #..#.#..#.
  * ..####.###
  * ..#.#.###.
  * ...#.#.#.#
  *
  * Tile 2729:
  * ...#.#.#.#
  * ####.#....
  * ..#.#.....
  * ....#..#.#
  * .##..##.#.
  * .#.####...
  * ####.#.#..
  * ##.####...
  * ##..#.##..
  * #.##...##.
  *
  * Tile 3079:
  * #.#.#####.
  * .#..######
  * ..#.......
  * ######....
  * ####.#..#.
  * .#...#.##.
  * #.#####.##
  * ..#.###...
  * ..#.......
  * ..#.###...
  *
  * By rotating, flipping, and rearranging them, you can find a square arrangement that causes all adjacent borders to line up:
  *
  * #...##.#.. ..###..### #.#.#####.
  * ..#.#..#.# ###...#.#. .#..######
  * .###....#. ..#....#.. ..#.......
  * ###.##.##. .#.#.#..## ######....
  * .###.##### ##...#.### ####.#..#.
  * .##.#....# ##.##.###. .#...#.##.
  * #...###### ####.#...# #.#####.##
  * .....#..## #...##..#. ..#.###...
  * #.####...# ##..#..... ..#.......
  * #.##...##. ..##.#..#. ..#.###...
  *
  * #.##...##. ..##.#..#. ..#.###...
  * ##..#.##.. ..#..###.# ##.##....#
  * ##.####... .#.####.#. ..#.###..#
  * ####.#.#.. ...#.##### ###.#..###
  * .#.####... ...##..##. .######.##
  * .##..##.#. ....#...## #.#.#.#...
  * ....#..#.# #.#.#.##.# #.###.###.
  * ..#.#..... .#.##.#..# #.###.##..
  * ####.#.... .#..#.##.. .######...
  * ...#.#.#.# ###.##.#.. .##...####
  *
  * ...#.#.#.# ###.##.#.. .##...####
  * ..#.#.###. ..##.##.## #..#.##..#
  * ..####.### ##.#...##. .#.#..#.##
  * #..#.#..#. ...#.#.#.. .####.###.
  * .#..####.# #..#.#.#.# ####.###..
  * .#####..## #####...#. .##....##.
  * ##.##..#.. ..#...#... .####...#.
  * #.#.###... .##..##... .####.##.#
  * #...###... ..##...#.. ...#..####
  * ..#.#....# ##.#.#.... ...##.....
  *
  * For reference, the IDs of the above tiles are:
  *
  * 1951    2311    3079
  * 2729    1427    2473
  * 2971    1489    1171
  *
  * To check that you've assembled the image correctly, multiply the IDs of the four corner tiles together. If you do this with the assembled tiles from the example above, you get 1951 * 3079 * 2971 * 1171 = 20899048083289.
  *
  * Assemble the tiles into an image. What do you get if you multiply together the IDs of the four corner tiles?
  *
  * Your puzzle answer was 14129524957217.
  * --- Part Two ---
  *
  * Now, you're ready to check the image for sea monsters.
  *
  * The borders of each tile are not part of the actual image; start by removing them.
  *
  * In the example above, the tiles become:
  *
  * .#.#..#. ##...#.# #..#####
  * ###....# .#....#. .#......
  * ##.##.## #.#.#..# #####...
  * ###.#### #...#.## ###.#..#
  * ##.#.... #.##.### #...#.##
  * ...##### ###.#... .#####.#
  * ....#..# ...##..# .#.###..
  * .####... #..#.... .#......
  *
  * #..#.##. .#..###. #.##....
  * #.####.. #.####.# .#.###..
  * ###.#.#. ..#.#### ##.#..##
  * #.####.. ..##..## ######.#
  * ##..##.# ...#...# .#.#.#..
  * ...#..#. .#.#.##. .###.###
  * .#.#.... #.##.#.. .###.##.
  * ###.#... #..#.##. ######..
  *
  * .#.#.### .##.##.# ..#.##..
  * .####.## #.#...## #.#..#.#
  * ..#.#..# ..#.#.#. ####.###
  * #..####. ..#.#.#. ###.###.
  * #####..# ####...# ##....##
  * #.##..#. .#...#.. ####...#
  * .#.###.. ##..##.. ####.##.
  * ...###.. .##...#. ..#..###
  *
  * Remove the gaps to form the actual image:
  *
  * .#.#..#.##...#.##..#####
  * ###....#.#....#..#......
  * ##.##.###.#.#..######...
  * ###.#####...#.#####.#..#
  * ##.#....#.##.####...#.##
  * ...########.#....#####.#
  * ....#..#...##..#.#.###..
  * .####...#..#.....#......
  * #..#.##..#..###.#.##....
  * #.####..#.####.#.#.###..
  * ###.#.#...#.######.#..##
  * #.####....##..########.#
  * ##..##.#...#...#.#.#.#..
  * ...#..#..#.#.##..###.###
  * .#.#....#.##.#...###.##.
  * ###.#...#..#.##.######..
  * .#.#.###.##.##.#..#.##..
  * .####.###.#...###.#..#.#
  * ..#.#..#..#.#.#.####.###
  * #..####...#.#.#.###.###.
  * #####..#####...###....##
  * #.##..#..#...#..####...#
  * .#.###..##..##..####.##.
  * ...###...##...#...#..###
  *
  * Now, you're ready to search for sea monsters! Because your image is monochrome, a sea monster will look like this:
  *
  *                   #
  * #    ##    ##    ###
  *  #  #  #  #  #  #
  *
  * When looking for this pattern in the image, the spaces can be anything; only the # need to match. Also, you might need to rotate or flip your image before it's oriented correctly to find sea monsters. In the above image, after flipping and rotating it to the appropriate orientation, there are two sea monsters (marked with O):
  *
  * .####...#####..#...###..
  * #####..#..#.#.####..#.#.
  * .#.#...#.###...#.##.O#..
  * #.O.##.OO#.#.OO.##.OOO##
  * ..#O.#O#.O##O..O.#O##.##
  * ...#.#..##.##...#..#..##
  * #.##.#..#.#..#..##.#.#..
  * .###.##.....#...###.#...
  * #.####.#.#....##.#..#.#.
  * ##...#..#....#..#...####
  * ..#.##...###..#.#####..#
  * ....#.##.#.#####....#...
  * ..##.##.###.....#.##..#.
  * #...#...###..####....##.
  * .#.##...#.##.#.#.###...#
  * #.###.#..####...##..#...
  * #.###...#.##...#.##O###.
  * .O##.#OO.###OO##..OOO##.
  * ..O#.O..O..O.#O##O##.###
  * #.#..##.########..#..##.
  * #.#####..#.#...##..#....
  * #....##..#.#########..##
  * #...#.....#..##...###.##
  * #..###....##.#...##.##.#
  *
  * Determine how rough the waters are in the sea monsters' habitat by counting the number of # that are not part of a sea monster. In the above example, the habitat's water roughness is 273.
  *
  * How many # are not part of a sea monster?
  *
  * Your puzzle answer was 1649.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2020 extends Utils {

  val input: String = readResourceAsString("day20.txt")

  case class Tile(id: Int, image: Display[Char]) {
    val edges: Map[Direction, String] = Geom2dUtils.mainDirections
      .map{ dir =>
        (dir, image.edgePositionsByDirClockwise(dir).map(image.apply).mkString(""))
      }
      .toMap

    val flippedEdges: MapView[Direction, String] =
      edges.view.mapValues(_.reverse)

    val keys: Iterable[String] = edges.values ++ flippedEdges.values

    def rotate90clockwise: Tile =
      Tile(id, image.rotateClockwise90)
    def shrink1: Tile =
      Tile(id, image.shrinkBy(1))


    def transpose: Tile = Tile(id, image.transpose)
    // rotates and flips the Tile such that the left key is the one given.
    def rotateToLeft(leftKey: String): Tile = {
      val currentDirs = edges.filter(_._2 == leftKey).keys
      if(currentDirs.isEmpty)
        transpose.rotateToLeft(leftKey)
      else {
        val currentDir = currentDirs.head
        if(currentDir == Left)
          this
        else {
          rotate90clockwise.rotateToLeft(leftKey)
        }
      }
    }
    def rotateToTop(topKey: String): Tile =
      rotateToLeft(topKey).rotate90clockwise

  }

  val tiles: List[Tile] = input.split("\n\n")
    .map{ tile =>
      val lines = tile.split('\n')
      val int = raw"Tile (\d+):".r
      val int(indexStr) = lines.head
      val index = indexStr.toInt
      val display = Display.readCharDisplay(lines.tail.toSeq)
      Tile(index, display)
    }
    .toList

  val tileCount: Int = tiles.length
  val side: Int = math.sqrt(tileCount).toInt
  require(side * side == tileCount)

  val tileReverseIndex: Map[String, List[Tile]] = tiles
    .flatMap{tile => tile.keys
      .map{ key => (key, tile)
      }
    }
    .groupMap(_._1)(_._2)

  val uniqueEdges: collection.Set[String] =
    tileReverseIndex
      .view.mapValues(_.size)
      .filter(_._2 == 1)
      .keySet

  val cornerTiles: List[Tile] = tiles
    .filter(tile => tile.keys.count(uniqueEdges.contains) == 4)
  // 4 because every one is counted twice - forward and reversed; and we need two edged - top & left, top & right,...

  lazy val answer1: Long = {
//    println(tileCount)
//    println(tileReverseIndex.view.mapValues(_.size).toList.mkString("\n"))
//    println(uniqueEdges.toList.sorted.mkString("\n"))
//    println(s"cornerTiles.size =${cornerTiles.size}")
    cornerTiles.map(_.id.toLong).product
  }

  //Part 2
  @tailrec
  def align(tileReverseIndex: Map[String, List[Tile]], start: Tile): Display[Tile] = {
    val image = Display[Tile]((0,0), (side, side))()
    if(uniqueEdges.contains(start.edges(Up)) && uniqueEdges.contains(start.edges(Left))) {
      image((0, 0)) = start
      for{
        j <- image.ys
        i <- image.xs
        if i != 0 || j != 0
      }{
        if(i > 0) {
          val tileToLeft = image((i-1, j))
          val rightKey = tileToLeft.edges(Geom2dUtils.Right)
          val List(tile) = tileReverseIndex(rightKey).filterNot(_.id == tileToLeft.id)
          val aligned = tile.rotateToLeft(rightKey.reverse)
          image((i,j)) = aligned
        } else {
          val tileToTop = image((i, j-1))
          val bottomKey = tileToTop.edges(Geom2dUtils.Down)
          val List(tile) = tileReverseIndex(bottomKey).filterNot(_.id == tileToTop.id)
          val aligned = tile.rotateToTop(bottomKey.reverse)
          image((i,j)) = aligned
        }
      }
      image
    } else {
      println("rotating start tile")
      align(tileReverseIndex, start.rotate90clockwise)
    }
  }

  def shrinkAllAndMerge(tiles: Display[Tile]): Display[Char] =
    tiles
      .map(_
        .image
        .shrinkBy(1)
        .resetOffsetInPlace
      )
      .flatten[Char]

  val seaMonster: Display[Char] = Display.readCharDisplay(
    """                  #
      |#    ##    ##    ###
      | #  #  #  #  #  #
      |""".stripMargin.split('\n').toSeq,
    emptyChar = ' '
  )

  val seaMonsterPositions: List[(Int, Int)] =
    seaMonster.points.filter(p => seaMonster(p) == '#').toList

  @tailrec
  def rotateUntilFound(image: Display[Char], count: Int = 4): Option[(LazyList[Position], Display[Char])] = {
    val res = image.Pattern(seaMonsterPositions, '#').findAll
    if(res.isEmpty){
      if(count == 0)
        None
      else {
        println("rotating")
        rotateUntilFound(image.rotateClockwise90)
      }
    } else
      Some((res, image))
  }

  lazy val answer2: Int = {
    val aligned = align(tileReverseIndex, cornerTiles.head)
    val big = shrinkAllAndMerge(aligned)
//    println(big.showDisplay()(_.toString))
    val Some((positions, rotated)) = rotateUntilFound(big)
      .orElse(rotateUntilFound(big.transpose))
    for{
      topLeft <- positions
      relPos <- seaMonsterPositions
      pos = topLeft + relPos
    }{
      rotated(pos) = 'O'
    }
    rotated.count(_ == '#')
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
