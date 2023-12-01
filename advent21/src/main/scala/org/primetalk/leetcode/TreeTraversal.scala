package org.primetalk.leetcode


/**
 * Дано бинарное дерево, нужно найти и вернуть элемент,
 * находящийся на максимальной глубине от корня.
 */
object TreeTraversal {

    case class Node(
        value: Int, 
        left: Option[Node], 
        right: Option[Node]
    )

    def findDeepestNode(root: Node): Node = {
        type Depth = Int
        def loop(n: Node = root): (Node, Depth) = {
            val leftOpt = n.left.map(l => loop(l))
            val rightOpt = n.right.map(l => loop(l))
            (leftOpt, rightOpt) match {
                case (Some((leftNode, leftDepth)), Some((rightNode, rightDepth))) => 
                    if leftDepth > rightDepth then 
                      (leftNode, leftDepth + 1)
                    else
                      (rightNode, rightDepth + 1)
                case (Some((leftNode, leftDepth)), None) =>
                    (leftNode, leftDepth + 1)
                case (None, Some((rightNode, rightDepth))) => 
                    (rightNode, rightDepth + 1)
                case (None, None) =>
                    (n, 0)
            }
        }
        val (node, _) = loop()
        node
    }
}

