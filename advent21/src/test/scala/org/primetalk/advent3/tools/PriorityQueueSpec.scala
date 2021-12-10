package org.primetalk.advent3.tools

class PriorityQueueSpec extends UnitSpec:

  "PriorityQueue" should "return sorted" in {
    given Priority[Int] with
      def apply(i: Int): Int = i
      
    val examples = List(1,4,2,5,3,16,54,6,4,5)
    val result = PriorityQueue(Nil).insertAll(examples).toList
    result should equal (examples.sorted)
    
  }
