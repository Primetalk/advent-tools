package org.primetalk.leetcode

/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 * 
You are given an array of k linked-lists lists, each linked-list is sorted in ascending order.

Merge all the linked-lists into one sorted linked-list and return it.

 

Example 1:

Input: lists = [[1,4,5],[1,3,4],[2,6]]
Output: [1,1,2,3,4,4,5,6]
Explanation: The linked-lists are:
[
  1->4->5,
  1->3->4,
  2->6
]
merging them into one sorted list:
1->1->2->3->4->4->5->6

Example 2:

Input: lists = []
Output: []

Example 3:

Input: lists = [[]]
Output: []

 

Constraints:

    k == lists.length
    0 <= k <= 104
    0 <= lists[i].length <= 500
    -104 <= lists[i][j] <= 104
    lists[i] is sorted in ascending order.
    The sum of lists[i].length will not exceed 104.


 * 
 * 
 */
object MergeLists {
    class ListNode(_x: Int = 0, _next: ListNode = null) {
      var next: ListNode = _next
      var x: Int = _x
    }

    def mergeKLists(lists: Array[ListNode]): ListNode = {
        val indices = lists.indices
        @annotation.tailrec
        def loop(lists: Array[ListNode], reverseResult: List[Int] = Nil): ListNode = {
            if(lists.forall(_ == null)) {
                reverseResult.foldLeft(null: ListNode){ case (tail, i) => 
                    new ListNode(i, tail)
                }
            } else {
                val indexOfMin = indices.minBy{
                    i => 
                        val list = lists(i)
                        if(list == null)
                            Int.MaxValue
                        else
                            list.x
                }
                val list = lists(indexOfMin)
                lists(indexOfMin) = list.next
                loop(lists, list.x :: reverseResult)
            }
        }
        loop(lists = lists)
    }
}
