package org.primetalk.advent3.tools

import java.net.URL

import scala.io.Source
import scala.util.matching.Regex

trait Utils:

  def readResource(resourceName: String): Iterator[String] =
    readResourceLines(resourceName).iterator

  def readResourceLines(resourceName: String): IndexedSeq[String] =
    val resource: URL = getClass.getResource(resourceName)
    if resource == null then 
      throw IllegalArgumentException("couldn't find " + resourceName)
    val src = Source.fromURL(resource, "UTF-8")
    try
      src.getLines().toIndexedSeq
    finally
      src.close()

  def readResourceAsString(resourceName: String): String =
    val resource: URL = getClass.getResource(resourceName)
    if resource == null then 
      throw new IllegalArgumentException(s"Couldn't find resource '$resourceName'")
    else 
      val src = Source.fromURL(resource, "UTF-8")
      try
        src.mkString
      finally
        src.close()

  def thisObjectInputResourceName = 
    getClass.getSimpleName.replace("$", ".txt")

  def readThisObjectInput: String =
    readResourceAsString(thisObjectInputResourceName)

  def readThisObjectInputLines: IndexedSeq[String] =
    readResourceLines(thisObjectInputResourceName)

  val newLineRegex: Regex = "\n".r

  extension (text: String)
    def scalaLines: IndexedSeq[String] =
      text.split('\n').toIndexedSeq

  def splitLines(text: String): IndexedSeq[String] =
    newLineRegex.split(text).toIndexedSeq

  def parseIntsNewLineSeparated(text: String): IndexedSeq[Int] =
    splitLines(text).map(_.toInt)

  def parseIntsCommaSeparated(text: String): IndexedSeq[Int] =
    text.split(',').map(_.toInt).toIndexedSeq

  def parseLongsNewLineSeparated(text: String): IndexedSeq[Long] =
    splitLines(text).map(_.toLong)

  val numbersRegex: Regex = "[-]?\\d+".r

  def parseAllIntsInString(text: String): Seq[Int] =
    numbersRegex.findAllMatchIn(text)
      .map(_.toString().toInt).toSeq
  def parseAllLongsInString(text: String): Seq[Long] =
    numbersRegex.findAllMatchIn(text)
      .map(_.toString().toLong).toSeq

  def parseFirstIntInString(text: String): Int =
    numbersRegex
      .findFirstIn(text)
      .map(_.toInt)
      .getOrElse(throw IllegalArgumentException(s"no int in $text"))

object Utils extends Utils
