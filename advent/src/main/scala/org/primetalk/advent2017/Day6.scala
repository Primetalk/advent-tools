package org.primetalk.advent2017

/**
  * --- Day 6: Memory Reallocation ---
  *
  * A debugger program here is having an issue: it is trying to repair a memory reallocation routine, but it keeps getting stuck in an infinite loop.
  *
  * In this area, there are sixteen memory banks; each memory bank can hold any number of blocks. The goal of the reallocation routine is to balance the blocks between the memory banks.
  *
  * The reallocation routine operates in cycles. In each cycle, it finds the memory bank with the most blocks (ties won by the lowest-numbered memory bank) and redistributes those blocks among the banks. To do this, it removes all of the blocks from the selected bank, then moves to the next (by index) memory bank and inserts one of the blocks. It continues doing this until it runs out of blocks; if it reaches the last memory bank, it wraps around to the first one.
  *
  * The debugger would like to know how many redistributions can be done before a blocks-in-banks configuration is produced that has been seen before.
  *
  * For example, imagine a scenario with only four memory banks:
  *
  * The banks start with 0, 2, 7, and 0 blocks. The third bank has the most blocks, so it is chosen for redistribution.
  * Starting with the next bank (the fourth bank) and then continuing to the first bank, the second bank, and so on, the 7 blocks are spread out over the memory banks. The fourth, first, and second banks get two blocks each, and the third bank gets one back. The final result looks like this: 2 4 1 2.
  * Next, the second bank is chosen because it contains the most blocks (four). Because there are four memory banks, each gets one block. The result is: 3 1 2 3.
  * Now, there is a tie between the first and fourth memory banks, both of which have three blocks. The first bank wins the tie, and its three blocks are distributed evenly over the other three banks, leaving it with none: 0 2 3 4.
  * The fourth bank is chosen, and its four blocks are distributed such that each of the four banks receives one: 1 3 4 1.
  * The third bank is chosen, and the same thing happens: 2 4 1 2.
  *
  * At this point, we've reached a state we've seen before: 2 4 1 2 was already seen. The infinite loop is detected after the fifth block redistribution cycle, and so the answer in this example is 5.
  *
  * Given the initial block counts in your puzzle input, how many redistribution cycles must be completed before a configuration is produced that has been seen before?
  *
  * Your puzzle answer was 5042.
  * --- Part Two ---
  *
  * Out of curiosity, the debugger would also like to know the size of the loop: starting from a state that has already been seen, how many block redistribution cycles must be performed before that same state is seen again?
  *
  * In the example above, 2 4 1 2 is seen again after four cycles, and so the answer in that example would be 4.
  *
  * How many cycles are in the infinite loop that arises from the configuration in your puzzle input?
  *
  * Your puzzle answer was 1086.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day6 {
  val input = "5\t1\t10\t0\t1\t7\t13\t14\t3\t12\t8\t10\t7\t12\t0\t6"

  type Distribution = Seq[Int]

  def checksum(distribution: Distribution): Int =
    distribution.sum

  val initialDistribution: Distribution = input.split('\t').toSeq.map(_.toInt)

  val size: Int = initialDistribution.length

  val checksum1: Int = checksum(initialDistribution)

  require(size == 16)

  def redistribute(dist: Distribution): Distribution = {
    val m = dist.max
    val i = dist.indexOf(m)
    val l = m % size
    val b = m / size
    val res = dist.zipWithIndex.map{
      case (mm, j) if j == i =>
        mm + b + (if((j - i + size - 1) % size >= l) 0 else 1) - m
      case (mm, j) =>
        mm + b + (if((j - i + size - 1) % size >= l) 0 else 1)
    }
//    val check2 = checksum(res)
//    require(check2 == checksum1)
    res
  }

  def from(distribution: Distribution): LazyList[Distribution] =
    LazyList.cons(distribution, from(redistribute(distribution)))

  def findFirstDuplicateCount[T](stm: LazyList[T]): Int = {
    val (set2, _) =
      stm
        .scanLeft((Set[T](), false)){
          case ((set, _), d) =>
            (set + d, set.contains(d))
        }
        .dropWhile(!_._2)
        .head

    set2.size
  }

  def answer1: Int =
    findFirstDuplicateCount(from(initialDistribution))

  // Part 2

  def findFirstDuplicateCount2[T](stm: LazyList[T]): Int = {
    val (_, Some(dist), lst2) =
      stm
        .scanLeft((Set[T](), None: Option[T], List[T]())){
          case ((set, _, lst), d) =>
            (set + d, Some(d).filter(set.contains), d :: lst)
        }
        .dropWhile(_._2.isEmpty)
        .head

    lst2.tail.indexOf(dist) + 1
  }

  def answer2: Int =
    findFirstDuplicateCount2(from(initialDistribution))


  def main(args: Array[String]): Unit = {

    println(answer2)
  }
}
