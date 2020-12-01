package org.primetalk.advent2020

import org.primetalk.advent.tools.{SequenceUtils, Utils}

import scala.annotation.tailrec

/**
  *
  * https://adventofcode.com/2019/day/1
  *
  * --- Day 1: Report Repair ---
  *
  * After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island. Surely, Christmas will go on without you.
  *
  * The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.
  *
  * To save your vacation, you need to get all fifty stars by December 25th.
  *
  * Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
  *
  * Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.
  *
  * Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
  *
  * For example, suppose your expense report contained the following:
  *
  * 1721
  * 979
  * 366
  * 299
  * 675
  * 1456
  *
  * In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.
  *
  * Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?
  *
  * Your puzzle answer was 1014171.
  * --- Part Two ---
  *
  * The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the same criteria.
  *
  * Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.
  *
  * In your expense report, what is the product of the three entries that sum to 2020?
  *
  * Your puzzle answer was 46584630.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  *
  */
object Day2001 {

  lazy val input: List[Int] = List(
    1935,
    1956,
    1991,
    1425,
    1671,
    1537,
    1984,
    1569,
    1873,
    1840,
    1720,
    1937,
    1823,
    1625,
    1727,
    1812,
    1714,
    1900,
    1939,
    1931,
    1951,
    1756,
    1942,
    1611,
    1979,
    1930,
    1996,
    2000,
    1544,
    1780,
    1687,
    1760,
    1836,
    1814,
    1691,
    1817,
    1964,
    1899,
    1577,
    1547,
    866,
    1560,
    1988,
    1601,
    1970,
    1738,
    1507,
    1667,
    1851,
    1933,
    1515,
    1856,
    1969,
    1860,
    1801,
    2007,
    1866,
    1800,
    1749,
    1843,
    1711,
    1495,
    1905,
    763,
    1672,
    1858,
    1987,
    1492,
    1849,
    1993,
    1737,
    1874,
    1658,
    1810,
    1665,
    1768,
    1950,
    1879,
    1816,
    1868,
    1995,
    1763,
    1783,
    1833,
    1968,
    1847,
    1748,
    1725,
    1891,
    1755,
    286,
    1976,
    1977,
    1655,
    1808,
    1986,
    1779,
    1861,
    1953,
    1888,
    1792,
    1811,
    1872,
    1790,
    1839,
    1985,
    1827,
    1842,
    1925,
    1735,
    1635,
    1821,
    1820,
    1973,
    1531,
    1770,
    59,
    1846,
    1932,
    1907,
    1730,
    933,
    1395,
    1753,
    1751,
    361,
    1530,
    1782,
    1087,
    1589,
    1929,
    1795,
    1815,
    1732,
    1765,
    1877,
    1722,
    526,
    1709,
    1789,
    1892,
    1913,
    1662,
    1809,
    1670,
    1947,
    1835,
    1587,
    1758,
    1982,
    2009,
    1757,
    670,
    1983,
    1524,
    1878,
    1796,
    1952,
    566,
    1922,
    1882,
    1870,
    1799,
    1731,
    1724,
    1805,
    2003,
    1596,
    1566,
    1853,
    1911,
    1857,
    1739,
    1744,
    1627,
    1729,
    1745,
    1845,
    1582,
    1884,
    1883,
    1941,
    1764,
    1685,
    1791,
    1837,
    1697,
    1742,
    1781,
    1948,
    1876,
    1989,
    1643,
    1871,
    1906,
    1726,
    1958,
    1502,
    1927,
    1946,
  )

  /** Search such pairs that have sum equal to target. */
  def searchPairs(target: Int): List[(Int, Int)] = {
    val sorted: IndexedSeq[Int] = input.sorted.toIndexedSeq
    @tailrec
    def loop(left: Int, right: Int, pairs: List[(Int, Int)]): List[(Int, Int)] = {
      if (left >= right)
        pairs
      else {
        val l = sorted(left)
        val r = sorted(right)
        if (l + r == target)
          loop(left + 1, right - 1, (l, r) :: pairs)
        else if (l + r > target)
          loop(left, right - 1, pairs)
        else
          loop(left + 1, right, pairs)
      }
    }

    loop(0, sorted.length - 1, Nil)
  }

  lazy val answer1: List[Int] = {
    searchPairs(2020)
      .map(p => p._1 * p._2)
  }

  // Part 2
  // 59836089
  lazy val answer2: Seq[Int] = {
    for {
      first <- input
      p <- searchPairs(2020 - first)
      _ = println(s"$first, $p = ${first + p._1 + p._2}")
    } yield first * p._1 * p._2
  }


  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2.distinct)
  }
}
