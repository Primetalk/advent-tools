package org.primetalk.advent

import java.net.URL

import scala.io.Source
import scala.util.matching.Regex

trait Utils {

  def readResource(resourceName: String): Iterator[String] = {
    val resource: URL = getClass.getResource(resourceName)
    Source.fromURL(resource, "UTF-8").getLines
  }

  def readResourceAsString(resourceName: String): String = {
    val resource: URL = getClass.getResource(resourceName)
    Source.fromURL(resource, "UTF-8").mkString
  }

  val newLineRegex: Regex = "\n".r

  def splitLines(text: String): Seq[String] =
    newLineRegex.split(text)

  def parseIntsNewLineSeparated(text: String): Seq[Int] =
    splitLines(text).map(_.toInt)

  val intsRegex: Regex = "[-]?\\d+".r

  def parseAllIntsInString(text: String): Seq[Int] =
    intsRegex.findAllMatchIn(text).map{_.toString().toInt}.toSeq

  def unfold[A](z: A)(f: A => Option[A]): A = {
    @annotation.tailrec
    def unfold0(z: A): A = {
      f(z) match {
        case None => z
        case Some(zz) => unfold0(zz)
      }
    }
    unfold0(z)
  }

  //     val maxGen = 20
  //    def go(s: ExtendedState, gen: Int): ExtendedState = {
  //      if(gen == maxGen)
  //        s
  //      else
  //        go(generate(generativePatterns)(s, 22), gen + 1)
  //    }
  //    val state20 = go(state1, 0)
  //
  //    def from(s: ExtendedState): Stream[ExtendedState] =
  //      s #::
  //        from(generate(generativePatterns)(s, 22))
  //    val state20 = from(state1).drop(20).head
  def unfoldN[A](z: A, n: Int)(f: A => A): A = {
    @annotation.tailrec
    def unfoldN0(z: A, n: Int): A = {
      if(n == 0)
        z
      else
        unfoldN0(f(z), n - 1)
    }
    unfoldN0(z, n)
  }

  def unfoldWhile[A](z: A)(f: A => A, p: A => Boolean): A = {
    @annotation.tailrec
    def unfoldWhile0(z: A): A = {
      if(p(z))
        unfoldWhile0(f(z))
      else
        z
    }
    unfoldWhile0(z)
  }

  @annotation.tailrec
  final def unfoldUntil[A](f: A => A)(p: A => Boolean)(z: A): A = {
    if(p(z))
      z
    else
      unfoldUntil(f)(p)(f(z))
  }

  /** Starts with initial state `S`, then applies `f` continuously.
    * Each step adds something to output and updates state.
    * When `f` yields `None`, stops.
    *
    * O(N)
    */
  @annotation.tailrec
  final def unfoldWithSuffix[S, T](tail: List[T] = Nil)(f: S => Option[(S, T)])(z: S): List[T] = {
    f(z) match {
      case None => tail
      case Some((zz, b)) => unfoldWithSuffix(b :: tail)(f)(zz)
    }
  }

  def generateStreamFrom[S](s0: S)(f: S => S): Stream[S] =
    s0 #:: generateStreamFrom(f(s0))(f)
}
