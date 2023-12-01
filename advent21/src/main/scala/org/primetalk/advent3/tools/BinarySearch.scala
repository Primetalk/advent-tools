package org.primetalk.advent3.tools

// finds a position after which to insert the given value to maintain sorted order.
def findInsertionIndex(nums: IArraySlice[Int], value: Int): Int =

  def binarySearch(low: Int = 0, high: Int = nums.length - 1): Int =
    if high < low then -1
    else if nums(low) > value then low - 1
    else if nums(low) == value then low
    else if nums(high) <= value then high
    else
      val m = (low + high) / 2
      if m == low then low
      else if nums(m) >= value then binarySearch(low, m)
      else binarySearch(m, high)
  binarySearch()
