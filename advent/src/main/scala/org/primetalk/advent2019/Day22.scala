package org.primetalk.advent2019

import java.util

import org.primetalk.advent.tools.{PrimeNumbers, Utils}

import scala.annotation.tailrec

/**
  * https://adventofcode.com/2019/day/22
  *
  * --- Day 22: Slam Shuffle ---
  *
  * There isn't much to do while you wait for the droids to repair your ship. At least you're drifting in the right direction. You decide to practice a new card shuffle you've been working on.
  *
  * Digging through the ship's storage, you find a deck of space cards! Just like any deck of space cards, there are 10007 cards in the deck numbered 0 through 10006. The deck must be new - they're still in factory order, with 0 on the top, then 1, then 2, and so on, all the way through to 10006 on the bottom.
  *
  * You've been practicing three different techniques that you use while shuffling. Suppose you have a deck of only 10 cards (numbered 0 through 9):
  *
  * To deal into new stack, create a new stack of cards by dealing the top card of the deck onto the top of the new stack repeatedly until you run out of cards:
  *
  * Top          Bottom
  * 0 1 2 3 4 5 6 7 8 9   Your deck
  * New stack
  *
  * 1 2 3 4 5 6 7 8 9   Your deck
  * 0   New stack
  *
  * 2 3 4 5 6 7 8 9   Your deck
  * 1 0   New stack
  *
  * 3 4 5 6 7 8 9   Your deck
  * 2 1 0   New stack
  *
  * Several steps later...
  *
  * 9   Your deck
  * 8 7 6 5 4 3 2 1 0   New stack
  *
  * Your deck
  * 9 8 7 6 5 4 3 2 1 0   New stack
  *
  * Finally, pick up the new stack you've just created and use it as the deck for the next technique.
  *
  * To cut N cards, take the top N cards off the top of the deck and move them as a single unit to the bottom of the deck, retaining their order. For example, to cut 3:
  *
  * Top          Bottom
  * 0 1 2 3 4 5 6 7 8 9   Your deck
  *
  * 3 4 5 6 7 8 9   Your deck
  * 0 1 2                 Cut cards
  *
  * 3 4 5 6 7 8 9         Your deck
  * 0 1 2   Cut cards
  *
  * 3 4 5 6 7 8 9 0 1 2   Your deck
  *
  * You've also been getting pretty good at a version of this technique where N is negative! In that case, cut (the absolute value of) N cards from the bottom of the deck onto the top. For example, to cut -4:
  *
  * Top          Bottom
  * 0 1 2 3 4 5 6 7 8 9   Your deck
  *
  * 0 1 2 3 4 5           Your deck
  * 6 7 8 9   Cut cards
  *
  * 0 1 2 3 4 5   Your deck
  * 6 7 8 9               Cut cards
  *
  * 6 7 8 9 0 1 2 3 4 5   Your deck
  *
  * To deal with increment N, start by clearing enough space on your table to lay out all of the cards individually in a long line. Deal the top card into the leftmost position. Then, move N positions to the right and deal the next card there. If you would move into a position past the end of the space on your table, wrap around and keep counting from the leftmost card again. Continue this process until you run out of cards.
  *
  * For example, to deal with increment 3:
  *
  *
  * 0 1 2 3 4 5 6 7 8 9   Your deck
  * . . . . . . . . . .   Space on table
  * ^                     Current position
  *
  * Deal the top card to the current position:
  *
  * 1 2 3 4 5 6 7 8 9   Your deck
  * 0 . . . . . . . . .   Space on table
  * ^                     Current position
  *
  * Move the current position right 3:
  *
  * 1 2 3 4 5 6 7 8 9   Your deck
  * 0 . . . . . . . . .   Space on table
  * ^               Current position
  *
  * Deal the top card:
  *
  * 2 3 4 5 6 7 8 9   Your deck
  * 0 . . 1 . . . . . .   Space on table
  * ^               Current position
  *
  * Move right 3 and deal:
  *
  * 3 4 5 6 7 8 9   Your deck
  * 0 . . 1 . . 2 . . .   Space on table
  * ^         Current position
  *
  * Move right 3 and deal:
  *
  * 4 5 6 7 8 9   Your deck
  * 0 . . 1 . . 2 . . 3   Space on table
  * ^   Current position
  *
  * Move right 3, wrapping around, and deal:
  *
  * 5 6 7 8 9   Your deck
  * 0 . 4 1 . . 2 . . 3   Space on table
  * ^                 Current position
  *
  * And so on:
  *
  * 0 7 4 1 8 5 2 9 6 3   Space on table
  *
  * Positions on the table which already contain cards are still counted; they're not skipped. Of course, this technique is carefully designed so it will never put two cards in the same position or leave a position empty.
  *
  * Finally, collect the cards on the table so that the leftmost card ends up at the top of your deck, the card to its right ends up just below the top card, and so on, until the rightmost card ends up at the bottom of the deck.
  *
  * The complete shuffle process (your puzzle input) consists of applying many of these techniques. Here are some examples that combine techniques; they all start with a factory order deck of 10 cards:
  *
  * deal with increment 7
  * deal into new stack
  * deal into new stack
  * Result: 0 3 6 9 2 5 8 1 4 7
  *
  * cut 6
  * deal with increment 7
  * deal into new stack
  * Result: 3 0 7 4 1 8 5 2 9 6
  *
  * deal with increment 7
  * deal with increment 9
  * cut -2
  * Result: 6 3 0 7 4 1 8 5 2 9
  *
  * deal into new stack
  * cut -2
  * deal with increment 7
  * cut 8
  * cut -4
  * deal with increment 7
  * cut 3
  * deal with increment 9
  * deal with increment 3
  * cut -1
  * Result: 9 2 5 8 1 4 7 0 3 6
  *
  * Positions within the deck count from 0 at the top, then 1 for the card immediately below the top card, and so on to the bottom. (That is, cards start in the position matching their number.)
  *
  * After shuffling your factory order deck of 10007 cards, what is the position of card 2019?
  *
  * Your puzzle answer was 7665.
  * --- Part Two ---
  *
  * After a while, you realize your shuffling skill won't improve much more with merely a single deck of cards. You ask every 3D printer on the ship to make you some more cards while you check on the ship repairs. While reviewing the work the droids have finished so far, you think you see Halley's Comet fly past!
  *
  * When you get back, you discover that the 3D printers have combined their power to create for you a single, giant, brand new, factory order deck of 119315717514047 space cards.
  *
  * Finally, a deck of cards worthy of shuffling!
  *
  * You decide to apply your complete shuffle process (your puzzle input) to the deck 101741582076661 times in a row.
  *
  * You'll need to be careful, though - one wrong move with this many cards and you might overflow your entire ship!
  *
  * After shuffling your new, giant, factory order deck that many times, what number is on the card that ends up in position 2020?
  *
  * Your puzzle answer was 41653717360577.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day22 extends Utils {
  lazy val inputTextFromResource : Iterator[String] =
    readResource("day22.txt")

  sealed trait ShuffleTechnique
  case object `deal into new stack` extends ShuffleTechnique
  case class `deal with increment`(i: Int) extends ShuffleTechnique
  case class `cut`(i: Int) extends ShuffleTechnique

  def parseShuffleTechnique(line: String): ShuffleTechnique = {
    if(line.startsWith("cut"))
      `cut`(line.drop(4).toInt)
    else if(line.startsWith("deal into new stack"))
      `deal into new stack`
    else if(line.startsWith("deal with increment"))
      `deal with increment`(line.drop("deal with increment ".length).toInt)
    else
      throw new IllegalArgumentException("Couldn't parse " + line)
  }

  val sequenceOfShuffleTechniques: List[ShuffleTechnique] = inputTextFromResource.toList.map(parseShuffleTechnique)

  type Deck = Array[Int]

  def cutImpl(i: Int): Deck => Deck = a => {
    val n =
      if(i < 0)
        a.length + i
      else
        i
    val part1 = a.take(n)
    a.drop(n) ++ part1
  }

  def dealIntoNewStackImpl: Deck => Deck =
    a => a.reverse

  def dealWithIncrementImpl(n: Int): Deck => Deck = a => {
    val len = a.length
    val res = Array.fill(len)(0)
    @tailrec
    def loop(i: Int): Unit = if(i < len) {
      val pos = i*n % len
      res(pos) = a(i)
      loop(i + 1)
    }
    loop(0)
    res
  }

  def getShuffleImpl(s: ShuffleTechnique): Deck => Deck = s match {
    case `deal into new stack` => dealIntoNewStackImpl
    case `deal with increment`(i) =>dealWithIncrementImpl(i)
    case `cut`(i) => cutImpl(i)
  }

  def totalShuffleImpl(seq: List[ShuffleTechnique]): Deck => Deck =
    seq.map(getShuffleImpl).reduce(_.andThen(_))

  lazy val answer1: Int = {
//    val shuffleImpl = totalShuffleImpl(sequenceOfShuffleTechniques)
    val mod = 10007
    val factoryOrdered: Deck = (0 until mod).toArray

//    val lastDeck = shuffleImpl(factoryOrdered)
//    val ans1 = lastDeck.indexOf(2019)

//    val formulas = sequenceOfShuffleTechniques.map(getShuffleImplForward)
//    val shuffleImpl2 = formulas.reduce(_.andThen(_))
    val totalFormula = sequenceOfShuffleTechniques.foldLeft((ResidualFormula(1, 0, mod), factoryOrdered) ){ case ((formula, deck), shuffle) =>
      val res = getShuffleImplForward(shuffle)(formula)
      println(s"$shuffle: $res")
      val deck1 = res.toDeck
      val deck2 = getShuffleImpl(shuffle)(deck)
      assert(util.Arrays.equals(deck1, deck2))
      (res, deck2)
    }
    val ans2 = totalFormula._1(2019).toInt
    ans2
  }

  // Part 2

  // fast binary power algorithm
  def modPower(a: BigInt, n: BigInt, mod: BigInt): BigInt = {
    a.modPow(n, mod)
//    @scala.annotation.tailrec
//    def loop(current: BigInt, n: BigInt, mul: BigInt): BigInt = {
//      if(n == 1) {
//        current * mul % mod
//      } else {
//        val nextN = n / 2
//        val p2 = current * current % mod
//        loop(p2, nextN,
//          if(n % 2 == 1)
//            current * mul  % mod
//          else
//            mul
//        )
//      }
//    }
//    loop(a, n, 1)
  }

  /** This is a representation of a formula:
    * {{{
    * j = (m * i + offset) % mod
    * }}}
    *
    */
  case class ResidualFormula(m: BigInt, offset: BigInt, mod: BigInt) {
    require(offset < mod)
    require(m < mod)
    def apply(pos: BigInt): BigInt =
      (m*pos + offset) % mod

    def multiply(n: BigInt): ResidualFormula =
      ResidualFormula(m * n % mod, offset * n % mod, mod)
    def substitute(other: ResidualFormula): ResidualFormula =
      ResidualFormula(m * other.m % mod, (other.offset*m + offset) % mod, mod)

    def *(other: ResidualFormula): ResidualFormula =
        ResidualFormula(m * other.m % mod,
          (offset * other.m + (other.offset *m) + offset *other.offset) % mod,
          mod)

    // fast binary power algorithm
    def power(n: Long): ResidualFormula = {
      @scala.annotation.tailrec
      def loop(current: ResidualFormula, n: Long, mul: ResidualFormula): ResidualFormula = {
        if(n == 1) {
          current * mul
        } else {
          val nextN = n / 2
          val p2 = current * current
          loop(p2, nextN,
            if(n % 2 == 1)
              current * mul
            else
              mul
          )
        }
      }
      loop(this, n, ResidualFormula(1,0,mod))
    }

    // fast binary power algorithm
    def powerSubst(n: Long): ResidualFormula = {
      ResidualFormula(modPower(m, n, mod), (modPower(m, n, mod) - 1)*PrimeNumbers.modInverse(m - 1,mod)*offset % mod, mod)
    }
    def inverse: ResidualFormula = {
      val invM = PrimeNumbers.modInverse(m, mod)
      ResidualFormula(invM, (mod - offset) * invM % mod, mod)
    }

    def toDeck: Deck = {
      val res = Array.fill(mod.toInt)(0)
      for { i <- 0 until mod.toInt} {
        res(((m*i+offset)%mod).toInt) = i
      }
      res
    }
  }

  def cutImplForward(i: Int): ResidualFormula => ResidualFormula = {
    case ResidualFormula(m, offset, mod) =>
      ResidualFormula(m, (mod + offset - i) % mod, mod)
  }
  def cutImplReverse(i: Int): ResidualFormula => ResidualFormula = {
    case ResidualFormula(m, offset, mod) =>
      ResidualFormula(m, (offset + i) % mod, mod)
  }

  /**
    * j = (m * i + offset) % mod
    * newJ = mod - 1 - j
    * newJ = (mod - m * i - offset) %mod
    * @return
    */
  def dealIntoNewStackImplSymmetric: ResidualFormula => ResidualFormula = {
    case ResidualFormula(m, offset, mod) =>
      ResidualFormula((mod - m) % mod, (mod - 1  - offset) % mod, mod)
  }

  /** Find 1/n (% mod). GCD(n, mod) == 1 */
  def inverse(n: Long, mod: BigInt): Long = {
    modPower(n, mod - 2, mod).toLong // Fermat's little theorem
    //PrimeNumbers.modInverse(n, mod)
  }
  /**
    * j = (m * i + offset) % mod
    * newJ = j*n
    * newJ = (m * n * i + offset * n) % mod
    *
    * reverse = newJ/n - find m' such that m'*n = m
    *  */
  def dealWithIncrementImplReverse(n: Int): ResidualFormula => ResidualFormula = {
    {
      case ResidualFormula(m, offset, mod) =>
        val invN = inverse(n, mod)
        ResidualFormula((m * invN) % mod,(offset * invN) % mod, mod)
    }
  }
  def dealWithIncrementImplForward(n: Int): ResidualFormula => ResidualFormula = {
    {
      case ResidualFormula(m, offset, mod) =>
        ResidualFormula((m * n) % mod, offset * n % mod, mod)
    }
  }

  def getShuffleImpl2(s: ShuffleTechnique): ResidualFormula => ResidualFormula = s match {
    case `deal into new stack` => dealIntoNewStackImplSymmetric
    case `deal with increment`(i) => dealWithIncrementImplReverse(i)
    case `cut`(i) => cutImplReverse(i)
  }

  def getShuffleImplForward(s: ShuffleTechnique): ResidualFormula => ResidualFormula = s match {
    case `deal into new stack` => dealIntoNewStackImplSymmetric
    case `deal with increment`(i) => dealWithIncrementImplForward(i)
    case `cut`(i) => cutImplForward(i)
  }

  def totalShuffleImpl2(seq: List[ShuffleTechnique]): ResidualFormula => ResidualFormula =
    seq.reverse.map(getShuffleImpl2).reduce(_.andThen(_))

  lazy val answer2: Long = {
    val mod = 119315717514047L // prime number

    val shuffleImpl = sequenceOfShuffleTechniques.map(getShuffleImplForward).reduce(_.andThen(_))
    val totalFormula = shuffleImpl(ResidualFormula(1, 0, mod))

    val numberOfIterations = 101741582076661L // prime number
    val pos = 2020

    println(s"totalFormula=$totalFormula")
    val f = totalFormula.powerSubst(numberOfIterations)
    val invF = f.inverse
    println(s"invF=$invF")

    invF(pos).toLong
  }

  lazy val answer22: Long = {
    val mod = 119315717514047L // prime number
    val initialFormula = ResidualFormula(1,0,mod)
    val shuffleImpl = totalShuffleImpl2(sequenceOfShuffleTechniques)
    val totalReverseFormula = shuffleImpl(initialFormula)

    val numberOfIterations = 101741582076661L // prime number
    val pos = 2020

    println(s"totalReverseFormula=$totalReverseFormula")
    val f = totalReverseFormula.powerSubst(numberOfIterations)
    f(pos).toLong

  }


  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2) // 74566621524070 22495629027807 102343069500100 37392124385059 108894791356394 34137192041507 41653717360577
  }
}
