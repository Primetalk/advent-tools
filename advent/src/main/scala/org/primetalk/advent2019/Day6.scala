package org.primetalk.advent2019

import org.primetalk.advent.tools.GraphUtils._
import org.primetalk.advent.tools.{Display, Geom2dUtils, Utils}

/**
  * https://adventofcode.com/2019/day/6
  *
  */
object Day6 extends Utils {

  lazy val inputTextFromResource : Iterator[String] =
    readResource("day6.txt")

  type Id = String

  val rootId = "COM"

  lazy val edges: GraphEdges[Id] =
    inputTextFromResource.toSeq.map(parseOrbit)

  def parseOrbit(s: String): (Id, Id) = {
    val a = s.split(')')
    (a(0), a(1))
  }

  def allDependenciesCount(edges: GraphEdges[Id]): Int = {
//    val deps = convertEdgesToDirectDependenciesOnlyForTrees(edges) //.map{ case (_, s) => s.size }.sum
    val tree = convertEdgesToParentToTree(invertEdges(edges))
    val keys = tree.keys
    val pathLengths = keys.map(key =>
      (key, treeCountPathToRoot(tree, rootId)(key))
    )
    pathLengths.toSeq.map(_._2).sum
  }

  lazy val answer1: Long = {
    allDependenciesCount(edges)
  }

  lazy val answer2: Long = {
//    val deps = convertEdgesToDirectDependenciesOnlyForTrees(edges) //.map{ case (_, s) => s.size }.sum
    val tree = convertEdgesToParentToTree(invertEdges(edges))
    val path = treePath(tree, rootId)("YOU", "SAN")
    path.size - 3 // removing YOU (-1), SAN (-1) and counting steps instead of nodes (-1).
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2) // 353 guess; 352 answer
  }

}
