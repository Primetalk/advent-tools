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

  val spaces: Parser[Unit] = Parser.char(' ').rep.void

  def ws: Parser[Unit] = (Rfc5234.wsp | Rfc5234.lf).rep.void

  val decimalDigits: Parser[String] = Numbers.digits

  val positiveNumber: Parser[Int] = Numbers.nonNegativeIntString.map(_.toInt)

  val integer = 
    ((Parser.char('-')|Parser.char('+')).?.with1 ~ decimalDigits)
      .string
      .map(_.toInt)
      //  Numbers.bigInt.map(_.toInt)

  // sign is mandatory
  val signedInt = 
    ((Parser.char('-')|Parser.char('+')) ~ decimalDigits)
      .string
      .map(_.toInt)

  val positiveLong = decimalDigits.map(_.toLong)

  val positiveBigInt = decimalDigits.map(BigInt.apply)

  val word = Parser.charWhere(_.isLetter).rep.string

  val eol = Parser.char('\n')

  val stringUntilEol = Parser.charWhere(_ != '\n').string
  
  val endOfLine = stringUntilEol <* eol

  extension (str: String)
    /** Parses constant string as a fixed value.
      * Typically - enum value or ADT.
      */
    def parseAs[T](using Parser[T]): Either[cats.parse.Parser.Error, T] = 
      summon[Parser[T]].parseAll(str)
    def parseUnsafe[T](using Parser[T]): T =
      parseAs[T].fold(pe => 
        throw IllegalArgumentException(
          s"couldn't parse at offset ${pe.failedAtOffset}. "+
          s"Expected one of (${pe.expected.toList.mkString(", ")}. "+
          s"Offsets: ${pe.offsets.toList.mkString(",")}. Text at offset: ${str.substring(pe.failedAtOffset, math.min(pe.failedAtOffset + 10, str.length()))}"),
       identity)