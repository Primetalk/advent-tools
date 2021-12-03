package org.primetalk.advent3.tools

// import fastparse.{P, EagerOpsStr, EagerOps, ByNameOpsStr, ByNameOps}
import cats.parse.Parser
import cats.parse.Numbers
import cats.parse.Rfc5234.wsp
import cats.parse.Rfc5234
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
object ParsingUtils:
  def a = 0
//   type WhitespaceParser = P[_] => P[Unit]

  def spaces: Parser[Unit] = Parser.char(' ').rep.void

  def ws: Parser[Unit] = (Rfc5234.wsp | Rfc5234.lf).rep.void

  def decimalDigits: Parser[String] = Numbers.digits

  def positiveNumber: Parser[Int] = Numbers.nonNegativeIntString.map(_.toInt)

  def integer = 
    ((Parser.char('-')|Parser.char('+')).?.with1 ~ decimalDigits)
      .string
      .map(_.toInt)
      //  Numbers.bigInt.map(_.toInt)

  // sign is mandatory
  def signedInt = 
    ((Parser.char('-')|Parser.char('+')) ~ decimalDigits)
      .string
      .map(_.toInt)

  def positiveLong = decimalDigits.map(_.toLong)

  def positiveBigInt = decimalDigits.map(BigInt.apply)

  def word = Parser.charWhere(_.isLetter).rep.string

  extension (str: String)
    /** Parses constant string as a fixed value.
      * Typically - enum value or ADT.
      */
    def parseAs[T](using Parser[T]): Either[cats.parse.Parser.Error, T] = 
      summon[Parser[T]].parseAll(str)
