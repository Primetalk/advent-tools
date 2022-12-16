package org.primetalk.advent3.tools

case class Interval(start: Int, endInclusive: Int):
  val size: Int = endInclusive - start + 1

  def subtract(other: Interval): List[Interval] =
    if other.start > endInclusive || start > other.endInclusive then 
      List(this)
    else 
      List(
        Interval(start, other.start - 1),
        Interval(other.endInclusive + 1, endInclusive)
      )
        .filter(_.size > 0)

  def intersect(other: Interval): List[Interval] =
    if other.start > endInclusive || start > other.endInclusive then 
      List()
    else
      List(Interval(math.max(start, other.start), math.min(endInclusive, other.endInclusive)))

  def union(other: Interval): List[Interval] =
    if other.start > endInclusive + 1 || start > other.endInclusive + 1 then 
      List(this, other)
    else
      List(Interval(math.min(start, other.start), math.max(endInclusive, other.endInclusive)))

object Interval:
  def subtractAll(orig: List[Interval], toRemove: List[Interval]): List[Interval] =
    toRemove match
      case head :: tail => 
        subtractAll(orig.flatMap(_.subtract(head)), tail)
      case Nil =>
        orig

  def mergeAdjacent(lst: List[Interval], result: List[Interval] = Nil): List[Interval] =
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
  def unionAll(intervals: List[Interval], nonOverlapping: List[Interval] = Nil): List[Interval] =
    intervals match
      case head :: tail => 
        //val nonOverlapping2 = nonOverlapping.foldLeft(List(head))((lst, no) => lst.flatMap(no.union(_)))
        val nonOverlapping2 = 
          subtractAll(List(head), nonOverlapping) reverse_::: nonOverlapping
        unionAll(tail, nonOverlapping2)
        //unionAll(tail, mergeAdjacent(head::nonOverlapping.flatMap(_.union(head)))
      case Nil =>
        mergeAdjacent(nonOverlapping.sortBy(_.start))
    