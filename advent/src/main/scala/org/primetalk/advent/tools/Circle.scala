package org.primetalk.advent.tools

import scala.annotation.tailrec

/** A pair of lists that are considered as a circle.
  */
case class Circle[T](leftReversed: List[T], right: List[T], leftCount: Int, rightCount: Int) {
  def shakeRebalance(desiredRight: Int): Circle[T] = {
    println(s"shakeRebalance($desiredRight)")
    val s = size
    val newRightCount = math.max(s / 2, desiredRight)
    val allValue = right ::: leftReversed.reverse
    Circle(allValue.dropRight(newRightCount), allValue.take(newRightCount), 0, rightCount + leftCount)
  }
  def shakeToRight: Circle[T] = {
    println(s"shakeToRight")
    Circle(Nil, right ::: leftReversed.reverse, 0, rightCount + leftCount)
  }

  def shakeToLeft: Circle[T] = {
    println(s"shakeToLeft")
    Circle(leftReversed ::: right.reverse, Nil, rightCount + leftCount, 0)
  }

  def makeSureRight(i: Int): Circle[T] =
    if(rightCount >= i)
      this
    else
      shakeToRight

  def makeSureLeft(i: Int): Circle[T] =
    if(leftCount >= i)
      this
    else
      shakeToLeft

  def insertRight(el: T): Circle[T] = Circle(leftReversed, el :: right, leftCount, rightCount + 1)
  def insertLeft(el: T): Circle[T] = Circle(el::leftReversed, right, leftCount + 1, rightCount)

  def takeRight(n: Int): List[T] = right.take(n)
  def takeLeft(n: Int): List[T] = leftReversed.take(n)

  def dropRight(n: Int): Circle[T] = Circle(leftReversed, right.drop(n), leftCount, rightCount - n)
  def dropLeft(n: Int): Circle[T] = Circle(leftReversed.drop(n), right, leftCount - n, rightCount)

  def prepend(a: T, b: T, c: T, d: T): Circle[T] =
    Circle(leftReversed, a::b::c::d::right, leftCount, rightCount+4)

  def rightHead: T = right.head
  def leftHead: T = leftReversed.head

  final def goRight(n: Int): Circle[T] = {
    if(n == 0)
      this
    else {
      val m = n % size
      val Circle(ll, rr, lc, rc) = this.makeSureRight(m)
      val toMove = rr.take(m) // TODO: recursive implementation
      Circle(toMove reverse_::: ll, rr.drop(m), lc + m, rc - m)
    }
  }

  final def goLeft(n: Int): Circle[T] = {
    if(n == 0)
      this
    else {
      val m = n % size
      val Circle(ll, rr, lc, rc) = this.makeSureLeft(m)
      val toMove = ll.take(m) // TODO: recursive implementation
      Circle(ll.drop(m), toMove reverse_:::  rr, lc - m, rc + m)
    }
  }

  @tailrec
  final def moveRightUntil(pred: T => Boolean): Circle[T] = {
    val circle2 = makeSureRight(1)
    if(pred(circle2.rightHead))
      circle2
    else {
      val circle3 = circle2.goRight(1)
      circle3.moveRightUntil(pred)
    }
  }
  @tailrec
  final def moveLeftUntil(pred: T => Boolean): Circle[T] = {
    val circle2 = makeSureLeft(1)
    if(pred(circle2.leftHead))
      circle2
    else {
      val circle3 = circle2.goLeft(1)
      circle3.moveLeftUntil(pred)
    }
  }
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
  /** Go n to right or left. */
  def move(n: Int): Circle[T] = {
    if(n > 0)
      goRight(n)
    else if(n < 0)
      goLeft(-n)
    else
      this
  }

  def size: Int = leftCount + rightCount

  override def toString: String =
    leftReversed.reverse.mkString(",") + "^" + right.mkString(",")

  def map[B](f: T => B): Circle[B] =
    Circle(leftReversed.map(f), right.map(f), leftCount, rightCount)

  def toList: List[T] =
    shakeToRight.right
}

object Circle {
  def apply[T](as: T *): Circle[T] = Circle(List(), as.toList, 0, as.size)

}
