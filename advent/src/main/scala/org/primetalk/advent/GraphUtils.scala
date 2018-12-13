package org.primetalk.advent

import org.primetalk.advent.Geom2dUtils.{Position, PosOps, mainDirections, directions8}

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

  /** Inserts an element into otherwise sorted vector. */
  def insertIntoSortedVector[T: Ordering](v: Vector[T], el: T, prefix: List[T] = List()): Vector[T] = {
    lazy val h = v.head
    if(v.isEmpty)
      (el :: prefix).reverse.toVector
    else if(implicitly[Ordering[T]].gteq(h, el))
      (el :: prefix).foldLeft(v)(_.+:(_))
    else
      insertIntoSortedVector(v.tail, el, h :: prefix)
  }

  def findShortestPaths[T](
    graphAsFunction: GraphAsFunction[T],
    finish: Set[T]
  )(
    toVisit: Vector[(T, Int, ReversePath[T])],
    distances: Map[T, (Int, ReversePath[T])] = Map() // Int - the length of the path
  ): Set[(Int, ReversePath[T])] =
    if(toVisit.isEmpty)
      Set()
    else {
      val (h, length, hPath) = toVisit.head
      val hs = graphAsFunction(h)
      val paths: Set[(T, (Int, List[T]))] = hs.map(hh => (hh, (length + 1, hh :: hPath)))
      val inFinish: Set[(T, (Int, List[T]))] = paths.filter(p => finish.contains(p._1))
      if (inFinish.isEmpty) {
        val nextToVisit = paths
          .filterNot { case (hh, _) => distances.keySet.contains(hh) }
          .map { case (hh, (newLength, newPath)) => (hh, newLength, newPath) }
        val nextDistances = paths.foldLeft(distances){
          case (v, (hh, (ll, pp))) =>
            v.get(hh) match {
              case Some((l, _)) if l < ll => v
              case _ => v.updated(hh, (ll, pp))
            }
        }
        implicit val orderingByDistance: Ordering[(T, Int, ReversePath[T])] = Ordering.by(_._2)
        val nextToVisitSorted = nextToVisit.foldLeft(toVisit.tail)((v, el) => insertIntoSortedVector(v, el))
        findShortestPaths(graphAsFunction, finish)(nextToVisitSorted, nextDistances)
      } else
        inFinish.map(_._2)
    }

  /** Unconstrained graph for 2d plane. 4 main directions. */
  def d2graph4dir: GraphAsFunction[Position] =
    p => mainDirections.map(_ + p).toSet[Position]

  /** Unconstrained graph for 2d plane. 8 directions. */
  def d2graph8dir: GraphAsFunction[Position] =
    p => directions8.map(_ + p).toSet[Position]

  /** Calculates the minimum distance between nodes. */
  def distance[T](graphAsFunction: GraphAsFunction[T])(a: T, b: T): Int = {
    val paths = findShortestPaths(graphAsFunction, Set(b))(Vector((a, 0, a :: Nil)), Map())
    paths.head._1
  }

}
