package org.primetalk.advent2018

import fastparse._

/**
  * A few building blocks are defined for building custom parsers.
  */
object ParsingUtils {
  type WhitespaceParser = P[_] => P[Unit]

  def positiveNumber[_ : P](implicit whitespace: WhitespaceParser): P[Int] =
    P(CharPred(CharPredicates.isDigit).rep(1).!).map(_.toInt)

  def integer[_ : P](implicit whitespace: WhitespaceParser): P[Int] =
    P( ("-".? ~ CharPred(CharPredicates.isDigit).rep(1)).!).map(_.toInt)

  implicit class StringOps(str: String) {
    def parseAs[T, _ : P](value: T)(implicit whitespace: WhitespaceParser): P[T] =
      P(str.!).map(_ => value)
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
