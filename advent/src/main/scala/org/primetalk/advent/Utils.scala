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

}
object Utils extends Utils {}
