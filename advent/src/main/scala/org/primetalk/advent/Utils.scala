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

  def unfold[A](f: A => Option[A])(z: A): A = {
    f(z) match {
      case None => z
      case Some(zz) => unfold(f)(zz)
    }
  }

  /** Starts with initial state `S`, then applies `f` continuously.
    * Each step adds something to output and updates state.
    * When `f` yields `None`, stops.
    *
    * O(N)
    */
  def unfoldWithSuffix[S, T](tail: List[T] = Nil)(f: S => Option[(S, T)])(z: S): List[T] = {
    f(z) match {
      case None => tail
      case Some((zz, b)) => unfoldWithSuffix(b :: tail)(f)(zz)
    }
  }
}
