package org.primetalk.advent3.tools

import scala.annotation.tailrec
import cats.kernel.Order
import cats.collections.Heap

object GraphUtils:
  type Edge[T] = (T, T)

  type GraphEdges[T] = Seq[Edge[T]]

  type Predicate[T] = T => Boolean

  type GraphAsSetOfEdges[T] = Edge[T] => Boolean

  type GraphDependencies[T] = Map[T, Set[T]]

  type Tree[T] = Map[T, T] // map to parent

  // Seq is considered as Set of vertices.
  // The order is maintained
  type GraphAsFunction[T] = T => Seq[T]
  type GraphAsFunctionList[T] = T => List[T]

  type WeightedGraphAsFunction[T, W] = T => Seq[(T, W)]

  type ReversePath[T] = List[T]

  extension [T](deps: GraphDependencies[T])
    def add(s: T, e: T): GraphDependencies[T] =
      deps
        .updated(s, deps.getOrElse(s, Set(e)).+(e))
        .updated(e, deps.getOrElse(e, Set()))

    def addEdge(edge: Edge[T]): GraphDependencies[T] =
      deps
        .updated(edge._1, deps.getOrElse(edge._1, Set(edge._2)).+(edge._2))
        .updated(edge._2, deps.getOrElse(edge._2, Set()))

    def toFunction: GraphAsFunction[T] =
      p => deps.getOrElse(p, Set()).toSeq

  extension [T](edges: GraphEdges[T])
    def toDirectedGraph: GraphDependencies[T] =
      edges.foldLeft(Map[T, Set[T]]())(_.addEdge(_))

    /** Dependencies are directed backwards from end to start. */
    def toDependencies: GraphDependencies[T] =
      edges.invert.toDirectedGraph

    def invert: GraphEdges[T] =
      edges.map{ case (a,b) => (b,a) }

    def toParentTree: Tree[T] =
      @tailrec
      def loop(rest: List[(T,T)], tree: Tree[T]): Tree[T] = rest match {
        case Nil => tree
        case (child,parent)::t =>
          tree.get(child) match {
            case Some(existing) =>
              throw IllegalArgumentException(s"There are at least two parents for $child: Set($existing, $parent)")
            case None =>
              loop(t, tree.updated(child, parent))
          }
      }
      loop(edges.toList, Map())

    def toUndirectedGraph: GraphDependencies[T] = 
      (edges ++ edges.invert).toDirectedGraph    


  /**
    * Performs topological sort of list of elements.
    * The dependencies are represented by set of direct dependents for each node.
    *
    * @param dependencies dependency graph edges
    * @tparam T Ordering is used to select among independent nodes
    * @return the nodes in order that do not contradict the dependencies requirements.
    */
  @annotation.tailrec
  final def topologicalSortSubList[T: Ordering](listToSort: List[T], dependencies: GraphDependencies[T], acc: List[T] = List()): List[T] =
    if dependencies.isEmpty then
      acc.reverse
    else 
      val (noDependencies, hasDependencies) = listToSort.partition(dependencies(_).isEmpty)
      if noDependencies.isEmpty then
        throw new IllegalArgumentException("Cycle: " + hasDependencies.mkString(","))
      else 
        val found = noDependencies.min // removing just one at a time
        val nextList = listToSort.filterNot(_ == found)
        val nextDependencies = dependencies.collect {
          case (key, values) if key != found =>
            (key, values - found)
        }
        topologicalSortSubList(nextList, nextDependencies, found :: acc)

  final def topologicalSortFromDependencies[T: Ordering](dependencies: GraphDependencies[T], acc: List[T] = List()): List[T] = 
    topologicalSortSubList(dependencies.keys.toList, dependencies, acc)
       
  case class PartialSearchResult[P, R](news: List[P], found: List[R])
  /** 
   * Searches Rs in a P-space. 
   * Finishes when there are no news.
   * There is a global S that allows 
   * to eliminate some unwanted elements from search space.
   * It is used in two places - on generation side and on global elimination.
   */
  def findAllWidthFirst[P, R, S](f: S => P => PartialSearchResult[P, R], eliminate: (S, List[P]) => (S, List[P]))(zero: S, start: List[P], foundSoFar: List[R]): List[R] =
    if start.isEmpty then
      foundSoFar
    else
      val partialResults = start.map(f(zero))
      val nextBeforeElimination = partialResults.flatMap(_.news)
      val (s, next) = eliminate(zero, nextBeforeElimination)
      val nextFound = partialResults.map(_.found).foldLeft(foundSoFar)( (lst, f) => f reverse_::: lst )
      findAllWidthFirst(f, eliminate)(s, next, nextFound)

  case class PartialSearchResultWithPriority[P, R](news: MyPriorityQueue[P], found: List[R])

  def priorityFindFirst[P: Priority, R, S](
    f: S => P => PartialSearchResultWithPriority[P, R], 
    eliminate: (S, MyPriorityQueue[P]) => (S, MyPriorityQueue[P])
  )(zero: S, start: MyPriorityQueue[P], foundSoFar: List[R]): List[R] =
    if start.isEmpty then
      foundSoFar
    else 
      val (min, queue) = start.take
      val PartialSearchResultWithPriority(next, found) = f(zero)(min)
      found match
        case Nil =>
          val (zero2, nextElim) = eliminate(zero, next)
          val nextStartElim = nextElim ++ queue
          priorityFindFirst(f, eliminate)(zero2, nextStartElim, 
            found reverse_::: foundSoFar)
        case lst =>
          lst

  case class PartialSearchResultWithPriority2[P, R](news: Iterable[P], found: List[R])
  
  def priorityFindFirst2[P: Order, R, S](
    f: S => P => PartialSearchResultWithPriority2[P, R], 
    eliminate: (S, Iterable[P]) => (S, Iterable[P])
  )(zero: S, start: Heap[P], foundSoFar: List[R]): List[R] =
    start.pop match
     case None =>
      foundSoFar
     case Some(min, queue) =>
      val PartialSearchResultWithPriority2(next, found) = f(zero)(min)
      found match
        case Nil =>
          val (zero2, nextElim) = eliminate(zero, next)
          val nextStartElim = queue.addAll(nextElim)
          priorityFindFirst2(f, eliminate)(zero2, nextStartElim, 
            found reverse_::: foundSoFar)
        case lst =>
          lst

  def priorityFindAll2[P: Order, R, S](
    f: P => PartialSearchResultWithPriority2[P, R],
    estimate: (S, List[R]) => S, 
    eliminate: (S, Iterable[P]) => (S, Iterable[P])
  )(zero: S, start: Heap[P], foundSoFar: List[R]): List[R] =
    start.pop match
     case None =>
      foundSoFar
     case Some(min, queue) =>
      val PartialSearchResultWithPriority2(next, found) = f(min)
      val (zero2, nextElim) = eliminate(zero, next)
      val nextStartElim = queue.addAll(nextElim)
      val zero3 = estimate(zero2, found)
      priorityFindAll2(f, estimate, eliminate)(zero3, nextStartElim, 
        found reverse_::: foundSoFar)

  def priorityFindAll3[P: Order, R, S](
    f: P => PartialSearchResultWithPriority2[P, R],
    estimate: (S, List[R]) => S, 
    eliminate: (S, Iterable[P]) => (S, Iterable[P])
  )(zero: S, start: Heap[P], foundSoFar: List[R]): List[R] = {
    // cats.effect.MVar
    ???
  }

  case class PartialSearchResultWithOrdering[P, R](news: cats.collections.Heap[P], found: List[R])

  def orderingFindFirst[P: Ordering, R, S](
    f: S => P => PartialSearchResultWithOrdering[P, R], 
    eliminate: (S, cats.collections.Heap[P]) => (S, cats.collections.Heap[P])
  )(zero: S, start: cats.collections.Heap[P], foundSoFar: List[R]): List[R] =
    given Order[P] = Order.fromOrdering
    start.pop match
     case None =>  
      foundSoFar
     case Some(min, queue) =>
      val PartialSearchResultWithOrdering(next, found) = f(zero)(min)
      found match
        case Nil =>
          val (zero2, nextElim) = eliminate(zero, next)
          val nextStartElim = queue.addAll(nextElim.toList)
          orderingFindFirst(f, eliminate)(zero2, nextStartElim, 
            found reverse_::: foundSoFar)
        case lst =>
          lst

  /** This trait contains building blocks for searching shortest paths.
   * 
   * SearchSpace is defined by
   *  - graph (as a function)
   *  - target definition
   *  
   * @tparam T - Node type
   * @tparam W - weight
  */
  trait ShortestPathAlgorithms[T]:
    
    // final def findShortestPaths7(g: WeightedGraphAsFunction[T, W])(start: Set[T]): List[] =
    //   ???

    case class PathInfo(length: Int, reversePath: ReversePath[T]):
      def node: T = reversePath.head
      def prepend(h: T): PathInfo = 
        PathInfo(length + 1, h :: reversePath)

    val graphAsFunction: GraphAsFunction[T]

    val isFinish: T => Boolean

    given shorterIsPriority: Priority[PathInfo] = new Priority[PathInfo]:
      def apply(pi: PathInfo): Long = pi.length

    extension (shortestPath: Map[T, PathInfo])
      def thereIsShorterPath(path: PathInfo): Boolean = 
        shortestPath.get(path.node).exists(_.length <= path.length)
      def addFoundPaths(paths: List[PathInfo]): Map[T, PathInfo] =
        paths match 
          case Nil =>
            shortestPath
          case path::tail =>
            val updated = shortestPath.get(path.node) match
              case Some(PathInfo(existingLength, _)) if existingLength < path.length => shortestPath
              case _ => shortestPath.updated(path.node, path)
            updated.addFoundPaths(tail)
    /**
      * Searches for all shortest paths.
      *
      * @param toVisit - this should be initialized by all starting points we wish to consider.
      *                in particular,
      * @param lengthLimit - this is a signal that we have reached one of the finish vertices.
      *                    As soon as lengthLimit becomes <  Int.MaxValue
      *                    we only care about paths that are shorter.
      * @return the length of path and all found paths.
      */
    @tailrec
    final def findAllShortestPaths7(
      toVisitSortedByPathInfoLength: List[PathInfo],
      shortestPath: Map[T, PathInfo] = Map(),
      foundPaths: List[ReversePath[T]] = List(),
      priorityLimit: Long = Long.MaxValue
    )(using priority: Priority[PathInfo]): (Long, Seq[ReversePath[T]]) =

      toVisitSortedByPathInfoLength match 
        case Nil =>
          (priorityLimit, foundPaths)
        case headPath::tail =>
          if priority(headPath) >= priorityLimit then 
            // ignore this node, because it's further than lengthLimit
            findAllShortestPaths7(tail, shortestPath, foundPaths, priorityLimit)
          else
            val nextNodes: List[T] = graphAsFunction(headPath.node).toList
            val pathsToNextNodes: List[PathInfo] = nextNodes.map(headPath.prepend)
            val inFinish: List[PathInfo] =
              pathsToNextNodes.filter(p => isFinish(p.node))
            val found = inFinish.toList
            val nextFoundPaths = found.map(_.reversePath) reverse_::: foundPaths
            val nextLengthLimit = found.headOption.map(priority(_)).getOrElse(priorityLimit)
            require(nextLengthLimit <= priorityLimit, "priority <= priorityLimit")
            val eliminatePathLongerThanExisting = pathsToNextNodes
              .filterNot(shortestPath.thereIsShorterPath)
            val nextShortestPath = shortestPath.addFoundPaths(pathsToNextNodes) 
            given Ordering[PathInfo] = Ordering.by(priority.apply)
            val nextToVisitSorted = insertAllIntoSortedList(tail, eliminatePathLongerThanExisting)
            
            findAllShortestPaths7(
              nextToVisitSorted,
              nextShortestPath,
              nextFoundPaths, 
              nextLengthLimit)
          
  /** A setup for searching all paths.
   * Some nodes might be visited a few times.
   */
  trait AllPathsSearch[T]:

    def pathInfo(n: T): PathInfo = 
      PathInfo(1, List(n), Map(n -> 1))

    case class PathInfo(length: Int, reversePath: ReversePath[T], visits: Map[T, Int]):
      def node: T = reversePath.head
      def prepend(h: T): PathInfo = 
        PathInfo(length + 1, h :: reversePath, visits.updated(h, visits.getOrElse(h, 0) + 1))

    val graphAsFunction: GraphAsFunction[T]

    val isFinish: T => Boolean

    val allowedVisits: T => Int

    final def findAllPaths(
      toVisit: List[PathInfo],
      completedPaths: List[PathInfo]
    ): List[PathInfo] = 
      toVisit match
        case Nil => 
          completedPaths
        case h :: t =>
          if isFinish(h.node) then
            findAllPaths(t, h :: completedPaths)
          else 
            val outgoing = graphAsFunction(h.node).toList
            val outgoing2 = outgoing.filter(n => allowedVisits(n) > h.visits.getOrElse(n, 0))
            val paths = outgoing2.map(h.prepend)
            findAllPaths( paths reverse_::: t, completedPaths)

      
  /** A trait that contains some building blocks for width-first search.
   * 
   */
  trait Search[S]:
    /** Return possible next states. 
     * Similar to `graphAsFunction`.
     */
    def next(path: PathInfo): PartialSearchResult[PathInfo, PathInfo]

    case class PathInfo(length: Int, reversePath: ReversePath[S], counts: Map[S, Int]):
      def last: S = reversePath.head
      def prepend(h: S): PathInfo = 
        PathInfo(length + 1, h :: reversePath, counts.updatedWith(h)(_.map(_ + 1).orElse(Some(1))))

  