package org.primetalk.advent2018

/** A pair of lists that are considered as left
  * and right parts of a long sequence.
  * There are some operations convenient to deal
  * with elements of a long sequence at some position.
  *
  * See also `Zipper`.
  */
case class SplitList[T](left: List[T], right: List[T]) {

  def insertRight(el: T): SplitList[T] = SplitList(left, el :: right)
  def insertLeft(el: T): SplitList[T] = SplitList(el::left, right)

  def takeRight(n: Int): List[T] = right.take(n)
  def takeLeft(n: Int): List[T] = left.take(n)

  def dropRight(n: Int): SplitList[T] = SplitList(left, right.drop(n))
  def dropLeft(n: Int): SplitList[T] = SplitList(left.drop(n), right)
  def tails: SplitList[T] = SplitList(left.tail, right.tail)

  def toList: List[T] = left reverse_::: right

  def hasHeads: Boolean =
    left.nonEmpty && right.nonEmpty

  def heads: (T, T) = (left.head, right.head)
  def rightHead: T = right.head
  def leftHead: T = left.head

  /** Go at most n to right or left. */
  def move(n: Int): SplitList[T] = {
    def goRight(n: Int, l: List[T], r: List[T]): SplitList[T] = {
      if(n == 0)
        SplitList(l, r)
      else r match {
        case h :: t =>
          goRight(n - 1, h :: l, t)
        case Nil =>
          SplitList(l, r)
      }
    }
    def goLeft(n: Int, l: List[T], r: List[T]): SplitList[T] = {
      if(n == 0)
        SplitList(l, r)
      else l match {
        case h :: t =>
          goLeft(n - 1, t, h :: r)
        case Nil =>
          SplitList(l, r)
      }
    }
    if(n > 0)
      goRight(n, left, right)
    else if(n < 0)
      goLeft(-n, left, right)
    else
      this
  }

}

object SplitList {
  def apply[T](as: T *): SplitList[T] = SplitList(List(), as.toList)
}