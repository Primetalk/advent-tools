package org.primetalk.advent

object GraphUtils {

  type GraphEdges[T] = Seq[(T, T)]

  type GraphDependencies[T] = Map[T, Set[T]]

  type GraphAsFunction[T] = T => Set[T]

  type ReversePath[T] = List[T]

  def convertEdgesToDependencies[T](edges: GraphEdges[T]): GraphDependencies[T] =
    edges
      .foldLeft(Map[T, Set[T]]()) {
        case (acc, (s,e)) =>
          acc ++
            Map(
              s -> acc.getOrElse(s, Set()), // just add starting node to keys.
              e -> (acc.getOrElse(e, Set()) + s)
            )
      }
  /**
    * Performs topological sort of nodes.
    * The dependencies are represented by set of direct dependents for each node.
    *
    * @param dependencies dependency graph edges
    * @tparam T Ordering is used to select among independent nodes
    * @return the nodes in order that do not contradict the dependencies requirements.
    */
  @annotation.tailrec
  final def topologicalSortFromDependencies[T: Ordering](dependencies: GraphDependencies[T], acc: List[T] = List()): List[T] = {
    if(dependencies.isEmpty)
      acc.reverse
    else {
      val (noDependencies, hasDependencies) = dependencies.partition(_._2.isEmpty)
      if (noDependencies.isEmpty) {
        throw new IllegalArgumentException("Cycle: " + hasDependencies.mkString(","))
      } else {
        val found = noDependencies.keys.min // removing just one at a time
        val nextDependencies = dependencies.collect {
          case (key, values) if key != found =>
            (key, values - found)
        }
        topologicalSortFromDependencies(nextDependencies, found :: acc)
      }
    }
  }

  def topologicalSortFromEdges[T: Ordering](edges: Seq[(T, T)]): List[T] =
    topologicalSortFromDependencies(
      convertEdgesToDependencies(edges)
    )

  /** Starts with a given list of nodes and walks through the given graph until
    * find the connected subgraph.
    */
  def renderFunctionalGraph[T](
    graphAsFunction: GraphAsFunction[T]
  )(
    toVisit: List[T],
    visited: Set[T] = Set(),
    knownSubGraph: GraphDependencies[T] = Map()
  ): GraphDependencies[T] = toVisit match {
    case Nil =>
      knownSubGraph
    case h :: t =>
      val hs: Set[T] = graphAsFunction(h)
      renderFunctionalGraph(graphAsFunction)(
        (hs -- visited).toList reverse_::: t,
        visited + h,
        knownSubGraph.updated(h, knownSubGraph.getOrElse(h, hs) ++ hs)
      )
  }

  def findShortestPaths[T](
    graphAsFunction: GraphAsFunction[T],
    finish: Set[T]
  )(
    toVisit: List[(T, Int, ReversePath[T])],
    visited: Map[T, (Int, ReversePath[T])] = Map()
  ): Set[(Int, ReversePath[T])] = toVisit match {
    case Nil => Set()
    case (h, length, hPath) :: t =>
      val hs = graphAsFunction(h)
      val notVisited = hs -- visited.keys
      if(notVisited.isEmpty)
        findShortestPaths(graphAsFunction, finish)(t, visited)
      else {
        val paths: Set[(T, (Int, List[T]))] = hs.map(hh => (hh, (length + 1, hh :: hPath)))
        val inFinish: Set[(T, (Int, List[T]))] = paths.filter(p => finish.contains(p._1))
        if (inFinish.isEmpty) {

          val nextVisited = paths.foldLeft(visited){ case (v, (hh, (ll, pp))) =>
            v.get(hh) match {
              case Some((l, _)) if l < ll => v
              case _ => v.updated(hh, (ll, pp))
            }
          }
          findShortestPaths(graphAsFunction, finish)(t, nextVisited)
        } else
          inFinish.map(_._2)
      }
  }

}
