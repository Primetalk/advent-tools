package org.primetalk.advent3.tools

import scala.reflect.ClassTag

case class IArraySlice[A](nums: IArray[A], left: Int = 0, right: Int = 0):
  def n = nums.length
  def length = n - left - right
  def isEmpty = length == 0
  def nonEmpty = length > 0

  def lastIndex = n - right - 1

  def last: A = nums(lastIndex)
  def head: A = nums(left)

  def apply(i: Int): A = nums(left + i)

  def lower1(i: Int): IArraySlice[A] = IArraySlice(nums, left, n - i - left - 1)
  def upper(i: Int): IArraySlice[A] = IArraySlice(nums, left + i, right)

  override def toString: String = s"[$left, $lastIndex]"

object ArraySlice:
  def empty[A: ClassTag] = IArraySlice[A](IArray[A](), 0, 0)
