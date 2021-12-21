package org.primetalk.advent2021

import org.primetalk.advent3.tools.Utils
import org.primetalk.advent3.tools.NumberSequenceUtils
import org.primetalk.advent3.tools.LongDistribution._
import org.primetalk.advent3.tools.LongDistribution

/**
  * https://adventofcode.com/2021/day/21
  * --- Day 21: Dirac Dice ---
  * 
  * There's not much to do as you slowly descend to the bottom of the ocean. The submarine computer challenges you to a nice game of Dirac Dice.
  * 
  * This game consists of a single die, two pawns, and a game board with a circular track containing ten spaces marked 1 through 10 clockwise. Each player's starting space is chosen randomly (your puzzle input). Player 1 goes first.
  * 
  * Players take turns moving. On each player's turn, the player rolls the die three times and adds up the results. Then, the player moves their pawn that many times forward around the track (that is, moving clockwise on spaces in order of increasing value, wrapping back around to 1 after 10). So, if a player is on space 7 and they roll 2, 2, and 1, they would move forward 5 times, to spaces 8, 9, 10, 1, and finally stopping on 2.
  * 
  * After each player moves, they increase their score by the value of the space their pawn stopped on. Players' scores start at 0. So, if the first player starts on space 7 and rolls a total of 5, they would stop on space 2 and add 2 to their score (for a total score of 2). The game immediately ends as a win for any player whose score reaches at least 1000.
  * 
  * Since the first game is a practice game, the submarine opens a compartment labeled deterministic dice and a 100-sided die falls out. This die always rolls 1 first, then 2, then 3, and so on up to 100, after which it starts over at 1 again. Play using this die.
  * 
  * For example, given these starting positions:
  * 
  * Player 1 starting position: 4
  * Player 2 starting position: 8
  * 
  * This is how the game would go:
  * 
  *     Player 1 rolls 1+2+3 and moves to space 10 for a total score of 10.
  *     Player 2 rolls 4+5+6 and moves to space 3 for a total score of 3.
  *     Player 1 rolls 7+8+9 and moves to space 4 for a total score of 14.
  *     Player 2 rolls 10+11+12 and moves to space 6 for a total score of 9.
  *     Player 1 rolls 13+14+15 and moves to space 6 for a total score of 20.
  *     Player 2 rolls 16+17+18 and moves to space 7 for a total score of 16.
  *     Player 1 rolls 19+20+21 and moves to space 6 for a total score of 26.
  *     Player 2 rolls 22+23+24 and moves to space 6 for a total score of 22.
  * 
  * ...after many turns...
  * 
  *     Player 2 rolls 82+83+84 and moves to space 6 for a total score of 742.
  *     Player 1 rolls 85+86+87 and moves to space 4 for a total score of 990.
  *     Player 2 rolls 88+89+90 and moves to space 3 for a total score of 745.
  *     Player 1 rolls 91+92+93 and moves to space 10 for a final score, 1000.
  * 
  * Since player 1 has at least 1000 points, player 1 wins and the game ends. At this point, the losing player had 745 points and the die had been rolled a total of 993 times; 745 * 993 = 739785.
  * 
  * Play a practice game using the deterministic 100-sided die. The moment either player wins, what do you get if you multiply the score of the losing player by the number of times the die was rolled during the game?
  * 
  * Your puzzle answer was 576600.
  * --- Part Two ---
  * 
  * Now that you're warmed up, it's time to play the real game.
  * 
  * A second compartment opens, this time labeled Dirac dice. Out of it falls a single three-sided die.
  * 
  * As you experiment with the die, you feel a little strange. An informational brochure in the compartment explains that this is a quantum die: when you roll it, the universe splits into multiple copies, one copy for each possible outcome of the die. In this case, rolling the die always splits the universe into three copies: one where the outcome of the roll was 1, one where it was 2, and one where it was 3.
  * 
  * The game is played the same as before, although to prevent things from getting too far out of hand, the game now ends when either player's score reaches at least 21.
  * 
  * Using the same starting positions as in the example above, player 1 wins in 444356092776315 universes, while player 2 merely wins in 341960390180808 universes.
  * 
  * Using your given starting positions, determine every possible outcome. Find the player that wins in more universes; in how many universes does that player win?
  * 
  * Your puzzle answer was 131888061854776.
  * 
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2121 extends Utils:

  // val player1Start = 4
  // val player2Start = 8
  val player1Start = 2
  val player2Start = 5

  case class DeterministicDice(current: Int, max: Int, count: Int):
    def roll: (Int, DeterministicDice) =
      (current, DeterministicDice(current % max + 1, max, count + 1))
    def take(n: Int, accum: List[Int] = Nil): (List[Int], DeterministicDice) = 
      if n == 0 then
        (accum.reverse, this)
      else
        val (d, next) = roll
        next.take(n - 1, d::accum)

  case class PlayerState(name: Int, position: Int, score: Int):
    def moveBySum(sum: Int): PlayerState =
      val space = (position + sum - 1) % 10 + 1
      val total = score + space
      // println(s"Player $name rolls $sum and moves to space $space for a total score of $total.")
      PlayerState(name,
        space,
        total
      )

    def move(dice: DeterministicDice): (PlayerState, DeterministicDice) =
      val (rolls, next) = dice.take(3)
      (moveBySum(rolls.sum),
        next)

  case class GameState(p1: PlayerState, p2: PlayerState, dice: DeterministicDice, turn1: Boolean):
    def isFinished: Boolean = 
      p1.score >= 1000 || p2.score >= 1000

    def move: GameState =
      if turn1 then
        val (p1m, dice1) = p1.move(dice)
        GameState(p1m, p2, dice1, !turn1)
      else
        val (p2m, dice1) = p2.move(dice)
        GameState(p1, p2m, dice1, !turn1)

  // 66528
  lazy val answer1: Int = 
    val initialState = GameState(PlayerState(1, player1Start, 0), PlayerState(2, player2Start, 0), DeterministicDice(1, 100, 0), true)
    val GameState(p1, p2, dice, turn1) = 
      NumberSequenceUtils.unfoldUntil[GameState](_.move)(_.isFinished)(initialState)
    if !turn1 then 
      p2.score * dice.count
    else
      p1.score * dice.count

  //Part 2
  val diracDiceOutcome: Distribution[Int] = 
    LongDistribution.fromList(List(1 -> 1L, 2 -> 1L, 3 -> 1L))

  val diracDiceOutcome3: Distribution[Int] = diracDiceOutcome * 3

  case class GameState2(p1: PlayerState, p2: PlayerState, turn1: Boolean):

    def isFinished: Boolean = 
      p1.score >= 21 || p2.score >= 21

    def move(dice: Int): GameState2 = 
      if turn1 then
        val p1m = p1.moveBySum(dice)
        GameState2(p1m, p2, !turn1)
      else
        val p2m = p2.moveBySum(dice)
        GameState2(p1, p2m, !turn1)

    def diracMove: Distribution[GameState2] = 
      if isFinished then
        LongDistribution.lift(this)
      else
        diracDiceOutcome3.dmap(this.move)

  // 8655029117788310208
  // 7908721963559028816
  lazy val answer2: Long =
    val initialState = GameState2(
      PlayerState(1, player1Start, 0), 
      PlayerState(2, player2Start, 0), 
      turn1 = true,
    )
    val initialStateDistribution = LongDistribution.lift(initialState)
    val stepsForAllToComplete = 21
    val finalStateDistribution = 
      NumberSequenceUtils.unfoldN[Distribution[GameState2]](_.dflatMap(_.diracMove))(initialStateDistribution, stepsForAllToComplete)
    assert(finalStateDistribution.forall(_.isFinished))
    assert(finalStateDistribution.forall(s => s.p1.score >= 21 ^ s.p2.score >= 21))
    val player1wins: Distribution[Boolean] = finalStateDistribution.dmap(_.p1.score >= 21)
    player1wins.toList.map(_._2).max

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
