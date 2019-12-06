package org.primetalk.advent2019

import org.primetalk.advent.tools.GraphUtils
import org.primetalk.advent.tools.GraphUtils.{Tree, convertEdgesToDirectDependenciesOnlyForTrees, treePath}
import org.primetalk.advent2019.Day6.{Id, edges, rootId}
import org.scalatest.{FlatSpec, Matchers}

class Day6Test extends FlatSpec with Matchers {

  behavior of "Day6Test"

  val input: Seq[String] = """COM)B
                |B)C
                |C)D
                |D)E
                |E)F
                |B)G
                |G)H
                |D)I
                |E)J
                |J)K
                |K)L
                |""".stripMargin
    .split('\n').toSeq
  val edges: Seq[(Id, Id)] = input.map(Day6.parseOrbit)
//  val deps = GraphUtils.convertEdgesToDirectDependenciesOnlyForTrees(edges)
  val tree: Tree[Id] = GraphUtils.convertEdgesToParentToTree(GraphUtils.invertEdges(edges))

  it should "edges" in {
    GraphUtils.treeCountPathToRoot(tree, "COM")("B") shouldBe 1
    GraphUtils.treeCountPathToRoot(tree, "COM")("C") shouldBe 2
    GraphUtils.treeCountPathToRoot(tree, "COM")("I") shouldBe 4
    Day6.allDependenciesCount(edges) shouldBe 42
  }

  it should "treePath" in {
    val edges2 = edges ++ List("K" -> "YOU", "I" -> "SAN")
    val path = treePath(GraphUtils.convertEdgesToParentToTree(GraphUtils.invertEdges(edges2)), rootId)("YOU", "SAN")
    path.size - 3 shouldBe 4

  }
}
