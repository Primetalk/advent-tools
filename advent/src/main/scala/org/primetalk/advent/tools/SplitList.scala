package org.primetalk.advent.tools

import scala.annotation.tailrec

/** A pair of lists that are considered as left
  * and right parts of a long sequence.
  * There are some operations convenient to deal
  * with elements of a long sequence at some position.
  *
  * See also `Zipper`.
  */
case class SplitList[T](leftReversed: List[T], right: List[T], leftCount: Int, rightCount: Int) {
//  assert(leftReversed.size == leftCount)
//  assert(right.size == rightCount)

  def insertRight(el: T): SplitList[T] = SplitList(leftReversed, el :: right, leftCount, rightCount + 1)
  def insertLeft(el: T): SplitList[T] = SplitList(el::leftReversed, right, leftCount + 1, rightCount)

  def takeRight(n: Int): List[T] = right.take(n)
  def takeLeft(n: Int): List[T] = leftReversed.take(n)

  def dropRight(n: Int): SplitList[T] = SplitList(leftReversed, right.drop(n), leftCount, rightCount - n)
  def dropLeft(n: Int): SplitList[T] = SplitList(leftReversed.drop(n), right, leftCount - n, rightCount)
  def tails: SplitList[T] = SplitList(leftReversed.tail, right.tail, leftCount - 1, rightCount - 1)

  def toList: List[T] = leftReversed reverse_::: right

  def toReversedList: List[T] = right reverse_::: leftReversed

  def hasHeads: Boolean =
    leftReversed.nonEmpty && right.nonEmpty

  def heads: (T, T) = (leftReversed.head, right.head)
  def rightHead: T = right.head
  def leftHead: T = leftReversed.head

  /** Go at most n to right or left. */
  def move(N: Int): SplitList[T] = {
    @tailrec
    def goRight(n: Int, l: List[T], r: List[T]): SplitList[T] = {
      if(n == 0)
        SplitList(l, r, leftCount + N, rightCount - N)
      else r match {
        case h :: t =>
          goRight(n - 1, h :: l, t)
        case Nil =>
          throw new IllegalArgumentException(s"cannot go right by $n ($N)")
        //          SplitList(l, r, leftCount + N, rightCount - N)
      }
    }
    @tailrec
    def goLeft(n: Int, l: List[T], r: List[T]): SplitList[T] = {
      if(n == 0)
        SplitList(l, r, leftCount + N, rightCount - N)
      else l match {
        case h :: t =>
          goLeft(n - 1, t, h :: r)
        case Nil =>
//          SplitList(l, r, leftCount + N, rightCount - N)
          throw new IllegalArgumentException(s"cannot go left by $n ($N)")
      }
    }
    if(N > 0)
      goRight(N, leftReversed, right)
    else if(N < 0)
      goLeft(-N, leftReversed, right)
    else
      this
  }

  def size: Int = leftCount + rightCount

  /** Searches both right and left. */
  final def findAround(pred: T => Boolean): Option[Int] = {
    @tailrec
    def loop(l: List[T], r: List[T], i: Int): Option[Int] = {
      l match {
        case Nil =>
          r match {
            case Nil =>
              None
            case ::(hr, rr) =>
              if(pred(hr))
                Some(i)
              else
                loop(Nil, rr, i + 1)
          }
        case ::(hl, ll) =>
          if(pred(hl))
            Some(- 1 - i)
          else {
            r match {
              case Nil => loop(ll, Nil, i + 1)
              case ::(hr, rr) =>
                if(pred(hr))
                  Some(i)
                else
                  loop(ll, rr, i + 1)
            }
          }
      }
    }
    loop(leftReversed, right, 0)
  }

  def append(list: List[T]): SplitList[T] =
    SplitList(leftReversed, right ::: list, leftCount, rightCount + list.size)

  def prependReversed(listReversed: List[T]): SplitList[T] =
    SplitList(leftReversed ::: listReversed, right, leftCount  + listReversed.size, rightCount)
}

object SplitList {
  def apply[T](as: T *): SplitList[T] = SplitList(List(), as.toList, 0, as.size)

  def empty[T]: SplitList[T] = SplitList(Nil, Nil, 0, 0)
}