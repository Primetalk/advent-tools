package org.primetalk.advent3.tools

import scala.annotation.tailrec
  
/** Inserts an element into the beginning of the sorted list.
 * PRE: list is sorted
 */
@tailrec
final def insertIntoSortedList[T: Ordering](list: List[T], el: T, prefix: List[T] = List()): List[T] =
  list match
    case Nil =>
      prefix reverse_::: el :: list
    case h :: tail =>
      import scala.math.Ordering.Implicits.infixOrderingOps
      if h >= el then
        prefix reverse_::: el :: list 
      else
        insertIntoSortedList(tail, el, h :: prefix)
// @typeclass
trait Priority[T]:
  def apply(t: T): Int

/**
 * Combines two sorted lists.
 * O(N + M)
 */
def mergeSorted[T: Ordering](sorted1: List[T], sorted2: List[T], accum: List[T] = Nil): List[T] =
  sorted1 match
    case Nil =>
      accum reverse_::: sorted2
    case h1::t1 =>
      sorted2 match
        case Nil =>
          accum reverse_::: sorted1
        case h2::t2 =>
          import Ordering.Implicits.infixOrderingOps
          if h1 < h2 then
            mergeSorted(t1, sorted2, h1 :: accum)
          else
            mergeSorted(sorted1, t2, h2 :: accum)
/** Inserts some elements into previously sorted list
 * O(N+M*ln M)
*/
def insertAllIntoSortedList[T: Ordering](list: List[T], elements: List[T]): List[T] =
  mergeSorted(list, elements.sorted)
  //elements. foldLeft(list){ case (lst, el) => insertIntoSortedList(lst, el)}


// given orderingByPriority[T](p: Priority[T]): Ordering[T] = 
//   Ordering

/** Allows to quickly extract minimal element.
 * This is not very efficient implementation.
 * One would prefer Chris Okasaki's one.
 */
case class MyPriorityQueue[T](sorted: List[T], unsorted: List[T] = Nil, minUnsortedPriority: Int = Int.MaxValue):
  def insert(el: T)(using priority: Priority[T]): MyPriorityQueue[T] =
    sorted match
      case Nil => 
        MyPriorityQueue((el :: unsorted).sortBy(priority(_)))
      case h :: t =>
        val pel = priority(el)
        if pel < priority(h) then
          MyPriorityQueue(el :: sorted, unsorted, minUnsortedPriority)
        else
          MyPriorityQueue(sorted, el :: unsorted, math.min(minUnsortedPriority, pel))

  def isEmpty: Boolean =
    sorted.isEmpty && unsorted.isEmpty

  def take(using priority: Priority[T]): (T, MyPriorityQueue[T]) =
    sorted match
      case Nil =>
        val (h :: sorted) = unsorted.sortBy(priority(_))
        (h, MyPriorityQueue(sorted))
      case h::t if priority(h) < minUnsortedPriority =>
        (h, MyPriorityQueue(t, unsorted, minUnsortedPriority))
      case _ :: _ =>
        given Ordering[T] = Ordering.by(priority(_))
        val (h :: sorted2) = insertAllIntoSortedList(sorted, unsorted)
        (h, MyPriorityQueue(sorted2))

  def insertAll(lst: List[T])(using priority: Priority[T]): MyPriorityQueue[T] =
    lst.foldLeft(this)(_.insert(_))

  def ++(other: MyPriorityQueue[T])(using priority: Priority[T]): MyPriorityQueue[T] =
    given Ordering[T] = Ordering.by(priority(_))
    MyPriorityQueue(
      mergeSorted(sorted, other.sorted), other.unsorted reverse_::: unsorted, 
      math.min(other.minUnsortedPriority, minUnsortedPriority)
    )

  def toList(using priority: Priority[T]): List[T] =
    def toListAcc(pq: MyPriorityQueue[T], acc: List[T] = Nil): List[T] =
      if pq.isEmpty then 
        acc.reverse
      else
        val (h, pq2) = pq.take
        toListAcc(pq2, h :: acc)
    toListAcc(this)
object MyPriorityQueue:
  def fromList[T](lst: List[T])(using priority: Priority[T]): MyPriorityQueue[T] =
    given Ordering[T] = Ordering.by(priority(_))
    MyPriorityQueue(lst.sorted)

  def empty[T]: MyPriorityQueue[T] = MyPriorityQueue(Nil)

// given MyHeap[MyPriorityQueue] with
//   def empty[T]: MyPriorityQueue[T] = 
//     MyPriorityQueue.empty

//   def enqueue[T: Ordering](h: MyPriorityQueue[T], el: T): MyPriorityQueue[T] = 
//     ???
//     // h.insert(el)

//   def dequeue[T: Ordering](h: MyPriorityQueue[T]): (T, MyPriorityQueue[T]) = 
//     h.take
    
//   def isEmpty[T: Ordering](h: MyPriorityQueue[T]): Boolean = 
//     h.isEmpty
