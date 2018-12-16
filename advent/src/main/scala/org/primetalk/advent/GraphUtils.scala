package org.primetalk.advent

import org.primetalk.advent.Geom2dUtils.{PosOps, Position, directions8, mainDirections}

import scala.collection.immutable

object GraphUtils {

  type GraphEdges[T] = Seq[(T, T)]

  type GraphDependencies[T] = Map[T, Set[T]]

  // Seq is considered as Set of vertices.
  // The order is maintained
  type GraphAsFunction[T] = T => Seq[T]

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
      val hs: Set[T] = graphAsFunction(h).toSet
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
  ): (Int, Seq[ReversePath[T]]) =
    if(toVisit.isEmpty)
      (Int.MaxValue, Seq())
    else {
      val (h, length, hPath) = toVisit.head
      val hs = graphAsFunction(h)
      val paths: Seq[(T, (Int, List[T]))] = hs.map(hh => (hh, (length + 1, hh :: hPath)))
      val inFinish: Seq[(T, (Int, List[T]))] = paths.filter(p => finish.contains(p._1))
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
      } else {
        (length, inFinish.map(_._2._2))
      }
    }

  /** Finds all shortest paths.
    * The implementation is not very efficient.
    */
  def findAllShortestPaths[T](
    graphAsFunction: GraphAsFunction[T],
    finish: Set[T]
  )(
    toVisit: Vector[(T, Int, List[ReversePath[T]])],
    distances: Map[T, (Int, List[ReversePath[T]])] = Map(),
    foundPaths: List[ReversePath[T]] = List(),
    lengthLimit: Int = Int.MaxValue
  ): (Int, Seq[ReversePath[T]]) =
    if(toVisit.isEmpty)
      (lengthLimit, foundPaths)
    else {
      val (h, length, hPaths) = toVisit.head
      val hs = graphAsFunction(h)
      val paths: Seq[(T, (Int, List[ReversePath[T]]))] =
        hs.map(hh => (hh, (length + 1, hPaths.map(hh :: _))))
        .filterNot(_._2._1 > lengthLimit)
      val inFinish: Seq[(T, (Int, List[ReversePath[T]]))] =
        paths.filter(p => finish.contains(p._1))
      val found = inFinish.map(_._2).toList
      val nextLengthLimit = found.headOption.map(_._1).getOrElse(lengthLimit)
      require(nextLengthLimit <= lengthLimit, "length <= lengthLimit")
      val nextToVisit = paths
        .filterNot{ case (hh, (len, _)) => distances.get(hh).exists(_._1 < len) }
        .map { case (hh, (newLength, newPath)) => (hh, newLength, newPath) }
      val nextDistances = paths.foldLeft(distances){
        case (v, (hh, (ll, pp))) =>
          v.get(hh) match {
            case Some((l, _)) if l < ll => v
            case Some((l, s)) if l == ll => v.updated(hh, (ll, s ++ pp))
            case _ => v.updated(hh, (ll, pp))
          }
      }
      implicit val orderingByDistance: Ordering[(T, Int, List[ReversePath[T]])] = Ordering.by(_._2)
      val nextToVisitSorted = nextToVisit.foldLeft(toVisit.tail)((v, el) =>
        insertIntoSortedVector(v, el))
      findAllShortestPaths(graphAsFunction, finish)(nextToVisitSorted, nextDistances,
        found.flatMap(_._2.toList) ::: foundPaths, nextLengthLimit)
    }

  case class PathInfo[T](length: Int, reversePath: ReversePath[T]) {
    def prepend(h: T): PathInfo[T] = PathInfo(length + 1, h :: reversePath)
  }
  /**
    * Specialized version of  findAllShortestPaths2 that is being used in day 15.
    *
    * @param toVisit - this should be initialized by all starting points we wish to consider.
    *                in particular,
    * @param lengthLimit - this is a signal that we have reached one of the finish vertices.
    *                    As soon as lengthLimit becomes <  Int.MaxValue
    *                    we only care about paths that are shorter.
    * @return the length of path and all found paths.
    */
  def findAllShortestPaths2[T](
    graphAsFunction: GraphAsFunction[T],
    finish: Set[T]
  )(
    toVisit: Vector[(T, PathInfo[T])],
    distances: Map[T, PathInfo[T]] = Map(),
    foundPaths: List[ReversePath[T]] = List(),
    lengthLimit: Int = Int.MaxValue
  ): (Int, Seq[ReversePath[T]]) =
    if(toVisit.isEmpty)
      (lengthLimit, foundPaths)
    else {
      val (h, path@PathInfo(length, hPath)) = toVisit.head
      if(length >= lengthLimit)
        findAllShortestPaths2(graphAsFunction, finish)(toVisit.tail, distances,
          foundPaths, lengthLimit)
      else {
        val hs: Seq[T] = graphAsFunction(h)
        val paths: Seq[(T, PathInfo[T])] =
          hs.map(hh => (hh, path.prepend(hh)))
        val inFinish: Seq[(T, PathInfo[T])] =
          paths.filter(p => finish.contains(p._1))
        val found = inFinish.map(_._2).toList
        val nextLengthLimit = found.headOption.map(_.length).getOrElse(lengthLimit)
        require(nextLengthLimit <= lengthLimit, "length <= lengthLimit")
        val nextToVisit = paths
          .filterNot { case (hh, PathInfo(len, _)) =>
            distances.get(hh).exists(_.length <= len) }
        val nextDistances = paths.foldLeft(distances) {
          case (v, (hh, np@PathInfo(ll, pp))) =>
            v.get(hh) match {
              case Some(PathInfo(l, _)) if l < ll => v
              case _ => v.updated(hh, np)
            }
        }
        implicit val orderingByDistance: Ordering[(T, PathInfo[T])] = Ordering.by(_._2.length)
        val nextToVisitSorted = nextToVisit.foldLeft(toVisit.tail)((v, el) =>
          insertIntoSortedVector(v, el))
        findAllShortestPaths2(graphAsFunction, finish)(nextToVisitSorted,
          nextDistances,
          found.map(_.reversePath) reverse_::: foundPaths, nextLengthLimit)
      }
    }
  /** Wrapper for calling findAllShortestPaths2. */
  def findAllShortestPaths3[T](
    graph: GraphAsFunction[T],
    start: T,
    finish: Set[T]
  ): (Int, Seq[ReversePath[T]]) = {
    val nextPositions = graph(start).toVector
    val shortestPathsStartingFromNextPositions: Seq[(Int, Seq[ReversePath[T]])] =
      for{
        p <- nextPositions
        pathInfo = PathInfo(1, p :: Nil)
        found = List(p).filter(finish.contains).map(p => p :: Nil)
      } yield
        findAllShortestPaths2[T](graph, finish)(
          Vector((p, pathInfo)),
          Map(
            start -> PathInfo(0, Nil),
            p -> pathInfo
          ),
          found,
          found.headOption.map(_ => 1).getOrElse(Int.MaxValue))
//    val found = nextPositions.filter(finish.contains)
//    findAllShortestPaths2[T](graph, finish)(
//      nextPositions.map(p => (p, PathInfo(1, p :: Nil))),
//      nextPositions.map(p => (p, PathInfo(1, p :: Nil))).toMap,
//      found.map(p => p :: Nil).toList,
//      found.headOption.map(_ => 1).getOrElse(Int.MaxValue))
    val length =
      if(shortestPathsStartingFromNextPositions.isEmpty)
        Int.MaxValue
      else
        shortestPathsStartingFromNextPositions.map(_._1).min
    (length, shortestPathsStartingFromNextPositions.filter(_._1 == length).flatMap(_._2))
  }
  /** Wrapper for calling findAllShortestPaths2. */
  def findAllShortestPaths4[T](
    graph: GraphAsFunction[T],
    start: T,
    finish: Set[T]
  ): (Int, Seq[ReversePath[T]]) = {
    findAllShortestPaths[T](graph, finish)(
      Vector((start, 0, Nil)), Map())

  }
  /** Unconstrained graph for 2d plane. 4 main directions. */
  def d2graph4dir: GraphAsFunction[Position] =
    p => mainDirections.map(_ + p)

  /** Unconstrained graph for 2d plane. 8 directions. */
  def d2graph8dir: GraphAsFunction[Position] =
    p => directions8.map(_ + p)

  /** Calculates the minimum distance between nodes. */
  def distance[T](graphAsFunction: GraphAsFunction[T])(a: T, b: T): Int = {
    val paths = findShortestPaths(graphAsFunction, Set(b))(Vector((a, 0, a :: Nil)), Map())
    paths._1
  }

}

