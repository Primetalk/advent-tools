package org.primetalk.advent2021

/** 
 * Given two sorted arrays nums1 and nums2 of size m and n respectively,
 * return the median of the two sorted arrays.
 * The overall run time complexity should be O(log (m+n)).
 */
object MedianOfTwoSortedArrays {
  /** Finds point to where to insert the value. */
  def findInsertionPoint(n: Int, arr: Array[Int])(min: Int = 0, max: Int = arr.length): Int =
    val probe = min + (max - min)/2
    if(probe >= arr.length)
      probe
    else
      val v = arr(probe)
      if(n == v)
        probe
      else if(n > v)
        findInsertionPoint(n, arr)(probe, max)
      else // (n < v)
        findInsertionPoint(n, arr)(min, probe)
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    val l1 = nums1.length
    val l2 = nums2.length
    val l = l1 + l2
    val p = l / 2 // indices of median in merged array
    val p2 = (l + 1) / 2
    if(l1 == 0) {
      (nums2(l2/2).toDouble + nums2((l2+1)/2).toDouble)/2
    } else if(l2 == 0) {
      findMedianSortedArrays(nums2, nums1)
    } else if(nums1(0) > nums2(0)) {
      findMedianSortedArrays(nums2, nums1)
    } else if(nums2(l2 - 1) > nums1(l1 - 1)) {
      def get(i: Int): Double =
        (
        if(i >= l1)
          nums2(i - l1)
        else
          nums1(i)
        ).toDouble
      (get(p) + get(p2))/2
    } else {
      assert(nums1(0) <= nums2(0))
      assert(l1 > 0)
      assert(l2 > 0)
      
      val i = findInsertionPoint(nums2(0), nums1)()
      assert(i < l1)
      val j = findInsertionPoint(nums1(l1 - 1), nums2)()
      assert(j >= 0)
      if(p2 < i) {
        (nums1(p).toDouble + nums1(p2))/2
      } else if(p > l1 + j) {
        (nums2(p - l1).toDouble + nums2(p2 - l1))/2
      } else {
        assert(p2 >= i)
        assert(p <= l1 + j)
        // the most interesting case
        ???
      }
      def find(i: Int, j: Int): Int = ???
      
      find(l1 / 2, l2 / 2)

    }
  }
}
