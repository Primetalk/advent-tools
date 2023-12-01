package org.primetalk.leetcode

import scala.reflect.ClassTag
import org.primetalk.advent3.tools.IArraySlice

def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
  val n1 = nums1.length
  val n2 = nums2.length
  val n = n1 + n2
  val medianIndex1 = (n - 1) / 2
  val medianIndex2 = n / 2

  val slice1 = IArraySlice(IArray.unsafeFromArray(nums1))
  val slice2 = IArraySlice(IArray.unsafeFromArray(nums2))

  def findElementAt(globalPos: Int): Int = {
    // INV: s1 and s2 head <= globalPos and last > globalPos
    @annotation.tailrec
    def loop(l1: Int, l2: Int): Int = {
      val relPos = globalPos - l1- l2
      assert(relPos >= 0)
      if(l1 == n1) nums2(globalPos - n1)
      else if(l2 == n2) nums1(globalPos - n2)
      else if(relPos == 0)
        math.min(nums1(l1), nums2(l2))
      else if(relPos == 1) {
        if(nums1(l1) < nums2(l2)) loop(l1 + 1, l2)
        else loop(l1, l2 + 1)
      } else {
        assert(relPos > 1)
        val i = relPos / 2
        require(i < n1 - l1 || i < n2 - l2)
        if(l1 + i < n1 && l2 + i < n2) {
          val v1 = nums1(l1 + i)
          val v2 = nums2(l2 + i)
          if(v1 < v2)
            loop(l1 + i, l2)
          else
            loop(l1, l2 + i)
        }
        else if(i < n1 - l1) 
          loop(l1 + i, l2)
        else// if(i < n2 - l2)
          loop(l1, l2 + i)
      }
    }
    loop(0,0)
  }
  (0.0 + findElementAt(medianIndex1) + findElementAt(medianIndex2)) / 2
}

def findMedianSortedArrays1(nums1: Array[Int], nums2: Array[Int]): Double = {
  val n1 = nums1.length
  val n2 = nums2.length
  val n = n1 + n2
  val medianIndex1 = (n - 1) / 2
  val medianIndex2 = n / 2

  val slice1 = IArraySlice(IArray.unsafeFromArray(nums1))
  val slice2 = IArraySlice(IArray.unsafeFromArray(nums2))

  def findElementAt(globalPos: Int): Int =
    // INV: s1 and s2 head <= globalPos and last > globalPos
    def loop(s1: IArraySlice[Int], s2: IArraySlice[Int]): Int = {
      val relPos = globalPos - s1.left - s2.left
      assert(relPos >= 0)
      if s1.isEmpty then s2(relPos)
      else if s2.isEmpty then s1(relPos)
      else if relPos == 0 then
        math.min(s1.head, s2.head)
      else if relPos == 1 then
        if s1.head < s2.head then loop(s1.upper(1), s2)
        else loop(s1, s2.upper(1))
      else
      // INV: s1.nonEmpty && s2.nonEmpty
      if s1.head > s2.head then loop(s2, s1)
      else
        // assert(s1.head <= s2.head)
        // if s1.last <= s2.head then {
        //   if relPos < s1.length then s1(relPos)
        //   else
        //     val j = relPos - s1.length
        //     s2(j)
        // } else {
          assert(s1.length > 1)
          assert(relPos > 1)
          val i = relPos / 2
          if i < s1.length then 
            loop(s1.upper(i), s2)
          else if i < s2.length then
            loop(s1, s2.upper(i))
          else 
            require(i < s1.length || i < s2.length)
            ???
        // }
    }
    loop(slice1, slice2)
  (0.0 + findElementAt(medianIndex1) + findElementAt(medianIndex2)) / 2
}

object Median:

  def main(args: Array[String]): Unit = {
    val res = findMedianSortedArrays(Array(1, 2), Array(4, 5, 6))
    // val res = findMedianSortedArrays(Array(1,2),Array(4,5,6))
    // val res = findMedianSortedArrays(Array(),Array(2,3))
    // val res = findMedianSortedArrays(Array(1,2,2),Array(1,2,3))
    println(res)
  }

          // val m1 = s1.length / 2
          // assert(m1 > 0)
          // val m1v = s1(m1)
          // def s1l = s1.lower1(m1)
          // def s1u = s1.upper(m1)
          // val m2 = s2.length / 2
          // val m2v = s2(m2)
          // val newPos = m1 + m2
          // val isLarger = newPos > relPos
          // def s2l = s2.lower1(m2)
          // def s2u = s2.upper(m2)
          // val `s1l overlaps s2u` = m1v > m2v
          // // if `s1l overlaps s2u` then
          // //   assert(s2u.last < s1u.head)
          // //   if isLarger then
          // //     loop(s1l, s2l)

          // // else
          // //   assert()
          // //   ???
          // // if (m1v > m2v) && (m1 + m2 > relPos) then
          // //   loop(s1.lower1(m1), )
          // // else
          // //   ???
          // if m2 == 0 then {
          //   assert(s2.length == 1)
          //   assert(s2.head == m2v)
          //   if m1 > relPos then
          //     if m1v > m2v /*s2.head*/ then loop(s1.lower1(m1), s2)
          //     else 
          //       s1(relPos) //loop(s1.lower1(0), s2.lower1(m2))
          //   else 
          //     loop(s1.upper(m1), s2)
          // }
          // else
          //   if m1 + m2 > relPos then
          //     if m1v > m2v then loop(s1.lower1(m1), s2)
          //     else loop(s1, s2.lower1(m2))
          //   else 
          //     if m1v > m2v then 
          //       loop(s1.upper(m1), s2)
          //     else 
          //       loop(s1, s2.upper(m2))
          // val i1v = s2.head
          // val i1 = findInsertionIndex(s1, i1v)
          // assert(i1 >= 0)
          // // val m1 = s1.length / 2
          // // val m1v = s1(m1)
          // if i1 > relPos then
          //   loop(s1.lower1(i1), s2)
          // else if i1 == 0 then
          //   ???
          // else
          //   loop(s1.upper(i1), s2)
          // if m1v <= s2.head then
          // else
          //   assert(m1 <= relPos)
          //   if m1v >= s2.last then loop(s1.lower1(m1), s2)
          //   else
          //     // m1 is inside s2
          //     val i2 = findInsertionIndex(s2, m1v)
          //     println(s"i2 = $i2")
          //     val v2 = s2(i2)
          //     if m1 + i2 > relPos then loop(s1.lower1(m1), s2)//.lower1(i2))
          //     else loop(s1.upper(m1), s2)//.upper(i2))
          //     // if m1v > v2
          //     // // val m2 = s2.length / 2
          //     // // val m2v = s2(m2)
          //     // ???
