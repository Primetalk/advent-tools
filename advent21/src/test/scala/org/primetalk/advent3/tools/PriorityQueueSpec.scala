package org.primetalk.advent3.tools

class PriorityQueueSpec extends UnitSpec:

  val examples = List(1, 4, 2, 5, 3, 16, 54, 6, 4, 5)
  
  val lst1 = 77 :: examples
  val lst2 = List(4, 12, 56, 66, 77)

  "PriorityQueue" should "return sorted" in {
    given Priority[Int] with
      def apply(i: Int): Long = i
      
    val queue =  MyPriorityQueue(Nil).insertAll(examples)
    val result = queue.toList
    result should equal (examples.sorted)
    val (min, q2) = queue.take
    min :: q2.toList should equal (examples.sorted)
  }
  "mergeSorted" should "merge" in {
    val result = mergeSorted(lst1.sorted, lst2.sorted)
    result should equal ((lst1 ++ lst2).sorted)
  }
