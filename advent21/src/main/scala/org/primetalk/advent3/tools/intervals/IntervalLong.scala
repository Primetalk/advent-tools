package org.primetalk.advent3.tools.intervals

import scala.annotation.tailrec

type Intervals = List[Interval]

type IntervalsFun = Intervals => Intervals

case class Interval(start: Long, endInclusive: Long):
  val length: Long = endInclusive - start + 1
  inline def from: Long = start
  inline def until: Long = endInclusive + 1

  def subtract(other: Interval): Intervals =
    if other.start > endInclusive || start > other.endInclusive then 
      List(this)
    else 
      List(
        Interval(start, other.start - 1),
        Interval(other.endInclusive + 1, endInclusive)
      )
        .filter(_.length > 0)

  def intersect(other: Interval): Intervals =
    if other.start > endInclusive || start > other.endInclusive then 
      List()
    else
      List(Interval(math.max(start, other.start), math.min(endInclusive, other.endInclusive)))
  def hasIntersection(other: Interval): Boolean = 
    start <= other.endInclusive && other.start <= endInclusive
  def union(other: Interval): Intervals =
    if other.start > endInclusive + 1 || start > other.endInclusive + 1 then 
      List(this, other)
    else
      List(Interval(math.min(start, other.start), math.max(endInclusive, other.endInclusive)))


  def shift(delta: Long): Interval = 
    Interval(start + delta, endInclusive + delta)
  def divideAt(pos: Long): Intervals =
    if pos <= from || pos > until then
      List(this)
    else
      List(Interval(from, pos - 1), Interval(pos, endInclusive))
  inline def isEmpty: Boolean = length == 0
  inline def nonEmpty: Boolean = length != 0 

object Interval:
  @tailrec
  def subtractAll(orig: Intervals, toRemove: Intervals): Intervals =
    toRemove match
      case head :: tail => 
        subtractAll(orig.flatMap(_.subtract(head)), tail)
      case Nil =>
        orig

  def intersectAll(orig: Intervals, other: Intervals): Intervals =
    orig.flatMap{ o =>
      other.flatMap(_.intersect(o))
    }.filterNot(_.isEmpty)

  @tailrec
  def mergeAdjacent(lst: Intervals, result: Intervals = Nil): Intervals =
    lst match
      case head1 :: head2 :: tail => 
        val lst = head1.union(head2)
        lst match
          case head :: Nil =>
            mergeAdjacent(head :: tail, result)
          case _ =>
            mergeAdjacent(head2 :: tail, head1 :: result)
      case head :: Nil => 
        (head :: result).reverse
      case Nil =>
        result.reverse
  // simplifies the collection of intervals to a collection of nonoverlapping intervals
  @tailrec
  def unionAll(intervals: Intervals, nonOverlapping: Intervals = Nil): Intervals =
    intervals match
      case head :: tail => 
        //val nonOverlapping2 = nonOverlapping.foldLeft(List(head))((lst, no) => lst.flatMap(no.union(_)))
        val nonOverlapping2 = 
          subtractAll(List(head), nonOverlapping) reverse_::: nonOverlapping
        unionAll(tail, nonOverlapping2)
        //unionAll(tail, mergeAdjacent(head::nonOverlapping.flatMap(_.union(head)))
      case Nil =>
        mergeAdjacent(nonOverlapping.sortBy(_.start))
    
  def minStart(intervals: Intervals): Long =
    intervals.map(_.start).min
