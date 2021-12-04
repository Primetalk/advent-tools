package org.primetalk.advent2021

import org.primetalk.advent3.tools.Utils
import org.primetalk.advent3.tools.IDisplay2D
import scala.annotation.tailrec

/**
  * https://adventofcode.com/2021/day/04
  * --- Day 4: Giant Squid ---
  * 
  * You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of your submarine.
  * 
  * Maybe it wants to play bingo?
  * 
  * Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)
  * 
  * The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). For example:
  * 
  * 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
  * 
  * 22 13 17 11  0
  *  8  2 23  4 24
  * 21  9 14 16  7
  *  6 10  3 18  5
  *  1 12 20 15 19
  * 
  *  3 15  0  2 22
  *  9 18 13 17  5
  * 19  8  7 25 23
  * 20 11 10 24  4
  * 14 21 16 12  6
  * 
  * 14 21 17 24  4
  * 10 16 15  9 19
  * 18  8 23 26 20
  * 22 11 13  6  5
  *  2  0 12  3  7
  * 
  * After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners, but the boards are marked as follows (shown here adjacent to each other to save space):
  * 
  * 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
  *  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
  * 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
  *  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
  *  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
  * 
  * After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still no winners:
  * 
  * 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
  *  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
  * 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
  *  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
  *  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
  * 
  * Finally, 24 is drawn:
  * 
  * 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
  *  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
  * 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
  *  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
  *  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
  * 
  * At this point, the third board wins because it has at least one complete row or column of marked numbers (in this case, the entire top row is marked: 14 21 17 24 4).
  * 
  * The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.
  * 
  * To guarantee victory against the giant squid, figure out which board will win first. What will your final score be if you choose that board?
  * 
  * Your puzzle answer was 44088.
  * --- Part Two ---
  * 
  * On the other hand, it might be wise to try a different strategy: let the giant squid win.
  * 
  * You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time counting its arms, the safe thing to do is to figure out which board will win last and choose that one. That way, no matter which boards it picks, it will win for sure.
  * 
  * In the above example, the second board is the last to win, which happens after 13 is eventually called and its middle column is completely marked. If you were to keep playing until this point, the second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.
  * 
  * Figure out which board will win last. Once it wins, what would its final score be?
  * 
  * Your puzzle answer was 23670.
  * 
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2104 extends Utils:

  val lines = readResourceLines("day04.txt")

  val numberSequence = lines.head.split(',').map(_.toInt).toList

  type Board = IDisplay2D[Int]

  def parseBoardArray(s: IndexedSeq[String]): IArray[IArray[Int]] =
    IArray.unsafeFromArray(
      s.filter(_.nonEmpty).map(
        _.split(' ')
          .filterNot(_.isEmpty)
          .map(_.toInt)
      )
      .map(IArray.unsafeFromArray)
      .toArray
    )

  def parseBoard(s: IndexedSeq[String]): Board =
    IDisplay2D((0,0), (5,5))(Some( () => parseBoardArray(s)))
  
  val boards = lines.tail.tail.sliding(6, 6).map(parseBoard).toSeq
  
  extension (board: Board)
    def playNumber(number: Int): Board =
      board
        .findAll(_ == number)
        .foldLeft(board)( (b, pos) => 
          b.updated(pos, 0)
        )
      
    def emptyRowExists: Boolean =
      board.ys.exists(y => board.linePositions(y).forall(board(_) == 0))
    
    def emptyColumnExists: Boolean =
      board.xs.exists(x => board.columnPositions(x).forall(board(_) == 0))

    def isWinning: Boolean = 
      emptyRowExists || emptyColumnExists
    def score: Int = 
      board.values.sum
    // plays sequence on the board. 
    // Returns the final board when it wins, index and the winning number.
    @tailrec
    def playSequence(sequence: List[Int], index: Int): (Board, Int, Int) = sequence match {
      case Nil => (board, index, -1)
      case number::tail => 
        val boardNext = board.playNumber(number)
        if boardNext.isWinning then 
          (boardNext, index, number)
        else
          boardNext.playSequence(tail, index + 1)
    }

  lazy val answer1: Int = 
    val boardsWithResults = boards.map(_.playSequence(numberSequence, 0))
    val (winner, _, winNumber) = boardsWithResults.minBy(_._2)
    winner.score * winNumber

  //Part 2
  // 2147482415, 72666, 22881, 23670
  lazy val answer2: Long =
    val boardsWithResults = boards.map(_.playSequence(numberSequence, 0))
    val (lastToWin, _, winNumber) = boardsWithResults.maxBy(_._2)
    lastToWin.score * winNumber


  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
