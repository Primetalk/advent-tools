package org.primetalk.advent3.tools

import java.net.URL

import scala.io.Source
import scala.util.matching.Regex

trait Utils:

  def readResource(resourceName: String): Iterator[String] =
    readResourceLines(resourceName).iterator

  def readResourceLines(resourceName: String): IndexedSeq[String] =
    val resource: URL = getClass.getResource(resourceName)
    val src = Source.fromURL(resource, "UTF-8")
    try {
      src.getLines().toIndexedSeq
    } finally {
      src.close()
    }

  def readResourceAsString(resourceName: String): String =
    val resource: URL = getClass.getResource(resourceName)
    val src = Source.fromURL(resource, "UTF-8")
    try {
      src.mkString
    } finally {
      src.close()
    }

  val newLineRegex: Regex = "\n".r

  def splitLines(text: String): IndexedSeq[String] =
    newLineRegex.split(text).toIndexedSeq

  def parseIntsNewLineSeparated(text: String): IndexedSeq[Int] =
    splitLines(text).map(_.toInt)

  def parseIntsCommaSeparated(text: String): IndexedSeq[Int] =
    text.split(',').map(_.toInt).toIndexedSeq

  def parseLongsNewLineSeparated(text: String): IndexedSeq[Long] =
    splitLines(text).map(_.toLong)

  val intsRegex: Regex = "[-]?\\d+".r

  def parseAllIntsInString(text: String): Seq[Int] =
    intsRegex.findAllMatchIn(text).map{_.toString().toInt}.toSeq


object Utils extends Utils
