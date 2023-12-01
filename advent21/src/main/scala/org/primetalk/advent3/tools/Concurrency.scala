package org.primetalk.advent3.tools

import cats.effect._
import cats.kernel.Order
import cats.collections.Heap
import cats.Parallel
// import cats.data.
import cats._, cats.syntax.all._
import cats.effect.unsafe.IORuntime

object Concurrency:

  val processorCoreCount = Runtime.getRuntime().availableProcessors()

  def popN[T:Order](h: Heap[T], n: Int, lst: List[T] = Nil): (List[T], Heap[T]) =
    if n == 0 then 
      (lst.reverse,h)
    else
      h.pop match
        case Some((t, h)) => popN(h, n-1, t::lst)
        case None => (lst.reverse, h)
      
  def enumerateSearchSpace[PointInSearchSpace: Order, AggregateResult](
    next: PointInSearchSpace => List[PointInSearchSpace],
    aggregate: (AggregateResult, PointInSearchSpace) => AggregateResult,
    eliminate: (AggregateResult, PointInSearchSpace) => Boolean,
    concurrency: Int = processorCoreCount
  )(using IORuntime)(
    toVisit: Heap[PointInSearchSpace], result: AggregateResult, 
    limit: Int = 1_000_000_000): AggregateResult =
    if limit <= 0 then 
      println(s"limit=$limit has been reached") // TODO: Either
      result
    else
      val (tops, tail) = popN(toVisit, concurrency)
      if tops.isEmpty then 
        result
      else
        val tops2 = tops.filterNot(eliminate(result, _))
        if tops2.isEmpty then 
          enumerateSearchSpace(next, aggregate, eliminate, concurrency)(tail, result, limit + 1)
        else
          val resultsNestedIO = IO.parSequenceN(concurrency)(
            tops2.map(h => 
              Spawn[IO].background(IO.delay(next(h)))
                .use( o => o.map(_.fold(???, throw _, identity)))
            )
          )
          // TraversableT
          val resultsIO = resultsNestedIO.flatMap(_.sequence)
          val nextPoints = resultsIO.unsafeRunSync().flatten
          //val nextPoints = tops2.flatMap(next)
          val result2 = tops2.foldLeft(result)(aggregate)
          enumerateSearchSpace(next, aggregate, eliminate, concurrency)(tail ++ nextPoints, result2, limit - nextPoints.size)

  def a: IO[Int] = ???
