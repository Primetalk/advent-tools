package org.primetalk.leetcode


import cats._, cats.implicits._, cats.collections._, cats.collections.syntax.all._

class MedianSpec extends org.primetalk.advent3.tools.UnitSpec:
  "findMedianSortedArrays" should "return median 4" in {
    findMedianSortedArrays(Array(1,2),Array(4,5,6)) should equal (4.0)
  }
  "findMedianSortedArrays" should "return median 2.0-3.5" in {
    findMedianSortedArrays(Array(1,2,3),Array(4,5,6)) should equal (3.5)
    findMedianSortedArrays(Array(),Array(1,2,3)) should equal (2.0)
    findMedianSortedArrays(Array(),Array(1,2,3,4)) should equal (2.5)
  }
  "findMedianSortedArrays" should "return median 123" in {
    findMedianSortedArrays(Array(1,2,3),Array(1,2,3)) should equal (2.0)
  }
  "findMedianSortedArrays" should "return median 2" in {
    findMedianSortedArrays(Array(1,2,2),Array(1,2,3)) should equal (2.0)
  }
  "findMedianSortedArrays" should "return median 3" in {
    findMedianSortedArrays(Array(2),Array(1,3,4,5)) should equal (3.0)
  }
  "findMedianSortedArrays" should "return median 3 2" in {
    findMedianSortedArrays(Array(1,3,4,5),Array(2)) should equal (3.0)
  }
  "findMedianSortedArrays" should "return median 3 5" in {
    findMedianSortedArrays(Array(2,4),Array(2, 4)) should equal (3.0)
  }
  "findMedianSortedArrays" should "return median 3 6" in {
    findMedianSortedArrays(Array(2,2,4,4),Array(2,2,4, 4)) should equal (3.0)
  }
  "findMedianSortedArrays" should "return median 3 7" in {
    findMedianSortedArrays(Array(2,2,2,4,4,4),Array(2,2,2,4,4, 4)) should equal (3.0)
  }
