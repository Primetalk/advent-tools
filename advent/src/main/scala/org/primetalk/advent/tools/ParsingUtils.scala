package org.primetalk.advent.tools

import fastparse._

/**
  * A few building blocks are defined for building custom parsers.
  * It's recommended to use NoWhitespace and explicit spaces.
  * Because in advent the format is fixed and it's more robust.
  * {{{
  *     import fastparse._
  *     import NoWhitespace._
  *     import org.primetalk.advent.tools.ParsingUtils._
  * }}}
  */
object ParsingUtils {
  type WhitespaceParser = P[_] => P[Unit]

  def spaces[_ : P](implicit whitespace: WhitespaceParser): P[Unit] =
    P(" ".rep(1))

  def ws[_ : P](implicit whitespace: WhitespaceParser): P[Unit] =
    P(" ".rep)

  def decimalDigits[_ : P](implicit whitespace: WhitespaceParser): P[String] =
    P( CharPred(CharPredicates.isDigit).rep(1).!)

  def positiveNumber[_ : P](implicit whitespace: WhitespaceParser): P[Int] =
    P(decimalDigits.map(_.toInt))

  def integer[_ : P](implicit whitespace: WhitespaceParser): P[Int] =
    P( (("-"|"+").? ~ decimalDigits).!).map(_.toInt)

  // sign is mandatory
  def signedInt[_ : P](implicit whitespace: WhitespaceParser): P[Int] = P(
    (("-"|"+") ~ decimalDigits).!
      .map(_.toInt)
  )

  def positiveLong[_ : P](implicit whitespace: WhitespaceParser): P[Long] =
    P(decimalDigits.map(_.toLong))

  def positiveBigInt[_ : P](implicit whitespace: WhitespaceParser): P[BigInt] =
    P(decimalDigits.map(BigInt.apply))

  def word[_ : P](implicit whitespace: WhitespaceParser): P[String] = P(
    CharPred(CharPredicates.isLetter).rep(1).!
  )

  implicit class StringOps(str: String) {
    /** Parses constant string as a fixed value.
      * Typically - enum value or ADT.
      */
    def parseAs[T, _ : P](value: T)(implicit whitespace: WhitespaceParser): P[T] =
      P(
        str.!
          .map(_ => value)
      )
  }

  /*
  // doesn't work for some reason
  def parseByTable[T, _ : P](table: Seq[(String, T)])(implicit whitespace: WhitespaceParser): P[T] = {
    val parsers: Seq[P[T]] = table.map {
      case (str, res) =>
        P(str.!).map(_ => res)
    }
    P(parsers.reduce(_ | _))
  }
  */
/*

  type Labelled[L, T] = (L, T)
  // doesn't work for some reason
  def parseLabelled[L, T, _ : P](label: P[L], value: P[T])(implicit whitespace: WhitespaceParser): P[(L, T)] =
    P(label ~ value)
*/
}
