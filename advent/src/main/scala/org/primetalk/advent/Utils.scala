package org.primetalk.advent

import java.net.URL

import scala.io.Source

trait Utils {

  def readResource(resourceName: String): Iterator[String] = {
    val resource: URL = getClass.getResource(resourceName)
    Source.fromURL(resource, "UTF-8").getLines
  }

}
