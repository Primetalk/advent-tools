package org.primetalk.advent2020

import org.primetalk.advent.tools.SequenceUtils

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
  * https://adventofcode.com/2020/day/22
  * {{{
  * --- Day 22: Crab Combat ---
  *
  * It only takes a few hours of sailing the ocean on a raft for boredom to sink in. Fortunately, you brought a small deck of space cards! You'd like to play a game of Combat, and there's even an opponent available: a small crab that climbed aboard your raft before you left.
  *
  * Fortunately, it doesn't take long to teach the crab the rules.
  *
  * Before the game starts, split the cards so each player has their own deck (your puzzle input). Then, the game consists of a series of rounds: both players draw their top card, and the player with the higher-valued card wins the round. The winner keeps both cards, placing them on the bottom of their own deck so that the winner's card is above the other card. If this causes a player to have all of the cards, they win, and the game ends.
  *
  * For example, consider the following starting decks:
  *
  * Player 1:
  * 9
  * 2
  * 6
  * 3
  * 1
  *
  * Player 2:
  * 5
  * 8
  * 4
  * 7
  * 10
  *
  * This arrangement means that player 1's deck contains 5 cards, with 9 on top and 1 on the bottom; player 2's deck also contains 5 cards, with 5 on top and 10 on the bottom.
  *
  * The first round begins with both players drawing the top card of their decks: 9 and 5. Player 1 has the higher card, so both cards move to the bottom of player 1's deck such that 9 is above 5. In total, it takes 29 rounds before a player has all of the cards:
  *
  * -- Round 1 --
  * Player 1's deck: 9, 2, 6, 3, 1
  * Player 2's deck: 5, 8, 4, 7, 10
  * Player 1 plays: 9
  * Player 2 plays: 5
  * Player 1 wins the round!
  *
  * -- Round 2 --
  * Player 1's deck: 2, 6, 3, 1, 9, 5
  * Player 2's deck: 8, 4, 7, 10
  * Player 1 plays: 2
  * Player 2 plays: 8
  * Player 2 wins the round!
  *
  * -- Round 3 --
  * Player 1's deck: 6, 3, 1, 9, 5
  * Player 2's deck: 4, 7, 10, 8, 2
  * Player 1 plays: 6
  * Player 2 plays: 4
  * Player 1 wins the round!
  *
  * -- Round 4 --
  * Player 1's deck: 3, 1, 9, 5, 6, 4
  * Player 2's deck: 7, 10, 8, 2
  * Player 1 plays: 3
  * Player 2 plays: 7
  * Player 2 wins the round!
  *
  * -- Round 5 --
  * Player 1's deck: 1, 9, 5, 6, 4
  * Player 2's deck: 10, 8, 2, 7, 3
  * Player 1 plays: 1
  * Player 2 plays: 10
  * Player 2 wins the round!
  *
  * ...several more rounds pass...
  *
  * -- Round 27 --
  * Player 1's deck: 5, 4, 1
  * Player 2's deck: 8, 9, 7, 3, 2, 10, 6
  * Player 1 plays: 5
  * Player 2 plays: 8
  * Player 2 wins the round!
  *
  * -- Round 28 --
  * Player 1's deck: 4, 1
  * Player 2's deck: 9, 7, 3, 2, 10, 6, 8, 5
  * Player 1 plays: 4
  * Player 2 plays: 9
  * Player 2 wins the round!
  *
  * -- Round 29 --
  * Player 1's deck: 1
  * Player 2's deck: 7, 3, 2, 10, 6, 8, 5, 9, 4
  * Player 1 plays: 1
  * Player 2 plays: 7
  * Player 2 wins the round!
  *
  *
  * == Post-game results ==
  * Player 1's deck:
  * Player 2's deck: 3, 2, 10, 6, 8, 5, 9, 4, 7, 1
  *
  * Once the game ends, you can calculate the winning player's score. The bottom card in their deck is worth the value of the card multiplied by 1, the second-from-the-bottom card is worth the value of the card multiplied by 2, and so on. With 10 cards, the top card is worth the value on the card multiplied by 10. In this example, the winning player's score is:
  *
  *    3 * 10
  * +  2 *  9
  * + 10 *  8
  * +  6 *  7
  * +  8 *  6
  * +  5 *  5
  * +  9 *  4
  * +  4 *  3
  * +  7 *  2
  * +  1 *  1
  * = 306
  *
  * So, once the game ends, the winning player's score is 306.
  *
  * Play the small crab in a game of Combat using the two decks you just dealt. What is the winning player's score?
  *
  * Your puzzle answer was 32401.
  * --- Part Two ---
  *
  * You lost to the small crab! Fortunately, crabs aren't very good at recursion. To defend your honor as a Raft Captain, you challenge the small crab to a game of Recursive Combat.
  *
  * Recursive Combat still starts by splitting the cards into two decks (you offer to play with the same starting decks as before - it's only fair). Then, the game consists of a series of rounds with a few changes:
  *
  *     Before either player deals a card, if there was a previous round in this game that had exactly the same cards in the same order in the same players' decks, the game instantly ends in a win for player 1. Previous rounds from other games are not considered. (This prevents infinite games of Recursive Combat, which everyone agrees is a bad idea.)
  *     Otherwise, this round's cards must be in a new configuration; the players begin the round by each drawing the top card of their deck as normal.
  *     If both players have at least as many cards remaining in their deck as the value of the card they just drew, the winner of the round is determined by playing a new game of Recursive Combat (see below).
  *     Otherwise, at least one player must not have enough cards left in their deck to recurse; the winner of the round is the player with the higher-value card.
  *
  * As in regular Combat, the winner of the round (even if they won the round by winning a sub-game) takes the two cards dealt at the beginning of the round and places them on the bottom of their own deck (again so that the winner's card is above the other card). Note that the winner's card might be the lower-valued of the two cards if they won the round due to winning a sub-game. If collecting cards by winning the round causes a player to have all of the cards, they win, and the game ends.
  *
  * Here is an example of a small game that would loop forever without the infinite game prevention rule:
  *
  * Player 1:
  * 43
  * 19
  *
  * Player 2:
  * 2
  * 29
  * 14
  *
  * During a round of Recursive Combat, if both players have at least as many cards in their own decks as the number on the card they just dealt, the winner of the round is determined by recursing into a sub-game of Recursive Combat. (For example, if player 1 draws the 3 card, and player 2 draws the 7 card, this would occur if player 1 has at least 3 cards left and player 2 has at least 7 cards left, not counting the 3 and 7 cards that were drawn.)
  *
  * To play a sub-game of Recursive Combat, each player creates a new deck by making a copy of the next cards in their deck (the quantity of cards copied is equal to the number on the card they drew to trigger the sub-game). During this sub-game, the game that triggered it is on hold and completely unaffected; no cards are removed from players' decks to form the sub-game. (For example, if player 1 drew the 3 card, their deck in the sub-game would be copies of the next three cards in their deck.)
  *
  * Here is a complete example of gameplay, where Game 1 is the primary game of Recursive Combat:
  *
  * === Game 1 ===
  *
  * -- Round 1 (Game 1) --
  * Player 1's deck: 9, 2, 6, 3, 1
  * Player 2's deck: 5, 8, 4, 7, 10
  * Player 1 plays: 9
  * Player 2 plays: 5
  * Player 1 wins round 1 of game 1!
  *
  * -- Round 2 (Game 1) --
  * Player 1's deck: 2, 6, 3, 1, 9, 5
  * Player 2's deck: 8, 4, 7, 10
  * Player 1 plays: 2
  * Player 2 plays: 8
  * Player 2 wins round 2 of game 1!
  *
  * -- Round 3 (Game 1) --
  * Player 1's deck: 6, 3, 1, 9, 5
  * Player 2's deck: 4, 7, 10, 8, 2
  * Player 1 plays: 6
  * Player 2 plays: 4
  * Player 1 wins round 3 of game 1!
  *
  * -- Round 4 (Game 1) --
  * Player 1's deck: 3, 1, 9, 5, 6, 4
  * Player 2's deck: 7, 10, 8, 2
  * Player 1 plays: 3
  * Player 2 plays: 7
  * Player 2 wins round 4 of game 1!
  *
  * -- Round 5 (Game 1) --
  * Player 1's deck: 1, 9, 5, 6, 4
  * Player 2's deck: 10, 8, 2, 7, 3
  * Player 1 plays: 1
  * Player 2 plays: 10
  * Player 2 wins round 5 of game 1!
  *
  * -- Round 6 (Game 1) --
  * Player 1's deck: 9, 5, 6, 4
  * Player 2's deck: 8, 2, 7, 3, 10, 1
  * Player 1 plays: 9
  * Player 2 plays: 8
  * Player 1 wins round 6 of game 1!
  *
  * -- Round 7 (Game 1) --
  * Player 1's deck: 5, 6, 4, 9, 8
  * Player 2's deck: 2, 7, 3, 10, 1
  * Player 1 plays: 5
  * Player 2 plays: 2
  * Player 1 wins round 7 of game 1!
  *
  * -- Round 8 (Game 1) --
  * Player 1's deck: 6, 4, 9, 8, 5, 2
  * Player 2's deck: 7, 3, 10, 1
  * Player 1 plays: 6
  * Player 2 plays: 7
  * Player 2 wins round 8 of game 1!
  *
  * -- Round 9 (Game 1) --
  * Player 1's deck: 4, 9, 8, 5, 2
  * Player 2's deck: 3, 10, 1, 7, 6
  * Player 1 plays: 4
  * Player 2 plays: 3
  * Playing a sub-game to determine the winner...
  *
  * === Game 2 ===
  *
  * -- Round 1 (Game 2) --
  * Player 1's deck: 9, 8, 5, 2
  * Player 2's deck: 10, 1, 7
  * Player 1 plays: 9
  * Player 2 plays: 10
  * Player 2 wins round 1 of game 2!
  *
  * -- Round 2 (Game 2) --
  * Player 1's deck: 8, 5, 2
  * Player 2's deck: 1, 7, 10, 9
  * Player 1 plays: 8
  * Player 2 plays: 1
  * Player 1 wins round 2 of game 2!
  *
  * -- Round 3 (Game 2) --
  * Player 1's deck: 5, 2, 8, 1
  * Player 2's deck: 7, 10, 9
  * Player 1 plays: 5
  * Player 2 plays: 7
  * Player 2 wins round 3 of game 2!
  *
  * -- Round 4 (Game 2) --
  * Player 1's deck: 2, 8, 1
  * Player 2's deck: 10, 9, 7, 5
  * Player 1 plays: 2
  * Player 2 plays: 10
  * Player 2 wins round 4 of game 2!
  *
  * -- Round 5 (Game 2) --
  * Player 1's deck: 8, 1
  * Player 2's deck: 9, 7, 5, 10, 2
  * Player 1 plays: 8
  * Player 2 plays: 9
  * Player 2 wins round 5 of game 2!
  *
  * -- Round 6 (Game 2) --
  * Player 1's deck: 1
  * Player 2's deck: 7, 5, 10, 2, 9, 8
  * Player 1 plays: 1
  * Player 2 plays: 7
  * Player 2 wins round 6 of game 2!
  * The winner of game 2 is player 2!
  *
  * ...anyway, back to game 1.
  * Player 2 wins round 9 of game 1!
  *
  * -- Round 10 (Game 1) --
  * Player 1's deck: 9, 8, 5, 2
  * Player 2's deck: 10, 1, 7, 6, 3, 4
  * Player 1 plays: 9
  * Player 2 plays: 10
  * Player 2 wins round 10 of game 1!
  *
  * -- Round 11 (Game 1) --
  * Player 1's deck: 8, 5, 2
  * Player 2's deck: 1, 7, 6, 3, 4, 10, 9
  * Player 1 plays: 8
  * Player 2 plays: 1
  * Player 1 wins round 11 of game 1!
  *
  * -- Round 12 (Game 1) --
  * Player 1's deck: 5, 2, 8, 1
  * Player 2's deck: 7, 6, 3, 4, 10, 9
  * Player 1 plays: 5
  * Player 2 plays: 7
  * Player 2 wins round 12 of game 1!
  *
  * -- Round 13 (Game 1) --
  * Player 1's deck: 2, 8, 1
  * Player 2's deck: 6, 3, 4, 10, 9, 7, 5
  * Player 1 plays: 2
  * Player 2 plays: 6
  * Playing a sub-game to determine the winner...
  *
  * === Game 3 ===
  *
  * -- Round 1 (Game 3) --
  * Player 1's deck: 8, 1
  * Player 2's deck: 3, 4, 10, 9, 7, 5
  * Player 1 plays: 8
  * Player 2 plays: 3
  * Player 1 wins round 1 of game 3!
  *
  * -- Round 2 (Game 3) --
  * Player 1's deck: 1, 8, 3
  * Player 2's deck: 4, 10, 9, 7, 5
  * Player 1 plays: 1
  * Player 2 plays: 4
  * Playing a sub-game to determine the winner...
  *
  * === Game 4 ===
  *
  * -- Round 1 (Game 4) --
  * Player 1's deck: 8
  * Player 2's deck: 10, 9, 7, 5
  * Player 1 plays: 8
  * Player 2 plays: 10
  * Player 2 wins round 1 of game 4!
  * The winner of game 4 is player 2!
  *
  * ...anyway, back to game 3.
  * Player 2 wins round 2 of game 3!
  *
  * -- Round 3 (Game 3) --
  * Player 1's deck: 8, 3
  * Player 2's deck: 10, 9, 7, 5, 4, 1
  * Player 1 plays: 8
  * Player 2 plays: 10
  * Player 2 wins round 3 of game 3!
  *
  * -- Round 4 (Game 3) --
  * Player 1's deck: 3
  * Player 2's deck: 9, 7, 5, 4, 1, 10, 8
  * Player 1 plays: 3
  * Player 2 plays: 9
  * Player 2 wins round 4 of game 3!
  * The winner of game 3 is player 2!
  *
  * ...anyway, back to game 1.
  * Player 2 wins round 13 of game 1!
  *
  * -- Round 14 (Game 1) --
  * Player 1's deck: 8, 1
  * Player 2's deck: 3, 4, 10, 9, 7, 5, 6, 2
  * Player 1 plays: 8
  * Player 2 plays: 3
  * Player 1 wins round 14 of game 1!
  *
  * -- Round 15 (Game 1) --
  * Player 1's deck: 1, 8, 3
  * Player 2's deck: 4, 10, 9, 7, 5, 6, 2
  * Player 1 plays: 1
  * Player 2 plays: 4
  * Playing a sub-game to determine the winner...
  *
  * === Game 5 ===
  *
  * -- Round 1 (Game 5) --
  * Player 1's deck: 8
  * Player 2's deck: 10, 9, 7, 5
  * Player 1 plays: 8
  * Player 2 plays: 10
  * Player 2 wins round 1 of game 5!
  * The winner of game 5 is player 2!
  *
  * ...anyway, back to game 1.
  * Player 2 wins round 15 of game 1!
  *
  * -- Round 16 (Game 1) --
  * Player 1's deck: 8, 3
  * Player 2's deck: 10, 9, 7, 5, 6, 2, 4, 1
  * Player 1 plays: 8
  * Player 2 plays: 10
  * Player 2 wins round 16 of game 1!
  *
  * -- Round 17 (Game 1) --
  * Player 1's deck: 3
  * Player 2's deck: 9, 7, 5, 6, 2, 4, 1, 10, 8
  * Player 1 plays: 3
  * Player 2 plays: 9
  * Player 2 wins round 17 of game 1!
  * The winner of game 1 is player 2!
  *
  *
  * == Post-game results ==
  * Player 1's deck:
  * Player 2's deck: 7, 5, 6, 2, 4, 1, 10, 8, 9, 3
  *
  * After the game, the winning player's score is calculated from the cards they have in their original deck using the same rules as regular Combat. In the above game, the winning player's score is 291.
  *
  * Defend your honor as Raft Captain by playing the small crab in a game of Recursive Combat using the same two decks as before. What is the winning player's score?
  *
  * Your puzzle answer was 31436.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  * }}}
  */
object Day2022 {

  type Player = Queue[Int]
  val Player1: Queue[Int] = Queue(
    40,
    26,
    44,
    14,
    3,
    17,
    36,
    43,
    47,
    38,
    39,
    41,
    23,
    28,
    49,
    27,
    18,
    2,
    13,
    32,
    29,
    11,
    25,
    24,
    35,
  )
  val Player2: Queue[Int] = Queue(
    19,
    15,
    48,
    37,
    6,
    34,
    8,
    50,
    22,
    46,
    20,
    21,
    10,
    1,
    33,
    30,
    4,
    5,
    7,
    31,
    12,
    9,
    45,
    42,
    16,
  )
  case class BoardState(player1: Player, player2: Player) {
    def isCompleted: Boolean =
      player1.isEmpty || player2.isEmpty

    def score: Long = {
      val player = player2.toList ::: player1.toList
      player.reverse.zipWithIndex.map(p => p._1.toLong * (p._2 + 1)).sum
    }
  }

  def turn(s0: BoardState): BoardState = {
//    println(s0)
    if(s0.isCompleted) {
      println("isCompleted")
      s0
    } else {
      val (t1, d1) = s0.player1.dequeue
      val (t2, d2) = s0.player2.dequeue
      if(t1 > t2)
        BoardState(d1.enqueue(t1).enqueue(t2), d2)
      else
        BoardState(d1, d2.enqueue(t2).enqueue(t1))
    }
  }

  // 31399, 32401
  lazy val answer1: Long = {
//    val init = State(Queue(9, 2, 6, 3, 1), Queue(5, 8, 4, 7, 10))
    val init = BoardState(Player1, Player2)
    val last = SequenceUtils.unfoldUntil(turn)(_.isCompleted)(init)
    println(last)
    last.score
  }

  //Part 2
  case class State2(board: BoardState, count1: Int, count2: Int, previous: Set[BoardState])
  type Winner = Int

  def recurseCombat(state: State2): (Winner, State2) =
    playGame(state)
  @tailrec
  def playGame(state: State2): (Winner, State2) = {
    if(state.previous.contains(state.board))
      (1, state)
    else {
      if(state.count1 == 0)
        (2, state)
      else if(state.count2 == 0)
        (1, state)
      else {
        val (t1, d1) = state.board.player1.dequeue
        val (t2, d2) = state.board.player2.dequeue
        val winner = if (state.count1 > t1 && state.count2 > t2) {
          //recurse combat
          val subGameState = State2(BoardState(d1.take(t1), d2.take(t2)), t1, t2, Set())
          val (winner, _) = recurseCombat(subGameState)
          winner
        } else {
          if (t1 > t2) 1 else 2
        }
        if (winner == 1)
          playGame(State2(BoardState(d1.enqueue(t1).enqueue(t2), d2), state.count1 + 1, state.count2 - 1, previous = state.previous + state.board))
        else
          playGame(State2(BoardState(d1, d2.enqueue(t2).enqueue(t1)), state.count1 - 1, state.count2 + 1, previous = state.previous + state.board))
      }
    }
  }
  lazy val answer2: Long = {
    val init = State2(BoardState(Player1, Player2), Player1.size, Player2.size, Set())
    val (_, last) = playGame(init)
    println(last)
    last.board.score
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
