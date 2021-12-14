package org.primetalk.advent2021

import org.primetalk.advent3.tools.Utils
import org.primetalk.advent3.tools.NumberSequenceUtils

/**
  * https://adventofcode.com/2021/day/14
  * --- Day 14: Extended Polymerization ---
  * 
  * The incredible pressures at this depth are starting to put a strain on your submarine. The submarine has polymerization equipment that would produce suitable materials to reinforce the submarine, and the nearby volcanically-active caves should even have the necessary input elements in sufficient quantities.
  * 
  * The submarine manual contains instructions for finding the optimal polymer formula; specifically, it offers a polymer template and a list of pair insertion rules (your puzzle input). You just need to work out what polymer would result after repeating the pair insertion process a few times.
  * 
  * For example:
  * 
  * NNCB
  * 
  * CH -> B
  * HH -> N
  * CB -> H
  * NH -> C
  * HB -> C
  * HC -> B
  * HN -> C
  * NN -> C
  * BH -> H
  * NC -> B
  * NB -> B
  * BN -> B
  * BB -> N
  * BC -> B
  * CC -> N
  * CN -> C
  * 
  * The first line is the polymer template - this is the starting point of the process.
  * 
  * The following section defines the pair insertion rules. A rule like AB -> C means that when elements A and B are immediately adjacent, element C should be inserted between them. These insertions all happen simultaneously.
  * 
  * So, starting with the polymer template NNCB, the first step simultaneously considers all three pairs:
  * 
  *     The first pair (NN) matches the rule NN -> C, so element C is inserted between the first N and the second N.
  *     The second pair (NC) matches the rule NC -> B, so element B is inserted between the N and the C.
  *     The third pair (CB) matches the rule CB -> H, so element H is inserted between the C and the B.
  * 
  * Note that these pairs overlap: the second element of one pair is the first element of the next pair. Also, because all pairs are considered simultaneously, inserted elements are not considered to be part of a pair until the next step.
  * 
  * After the first step of this process, the polymer becomes NCNBCHB.
  * 
  * Here are the results of a few steps using the above rules:
  * 
  * Template:     NNCB
  * After step 1: NCNBCHB
  * After step 2: NBCCNBBBCBHCB
  * After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
  * After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
  * 
  * This polymer grows quickly. After step 5, it has length 97; After step 10, it has length 3073. After step 10, B occurs 1749 times, C occurs 298 times, H occurs 191 times, and N occurs 865 times; taking the quantity of the most common element (B, 1749) and subtracting the quantity of the least common element (H, 161) produces 1749 - 161 = 1588.
  * 
  * Apply 10 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?
  * 
  * Your puzzle answer was 2975.
  * --- Part Two ---
  * 
  * The resulting polymer isn't nearly strong enough to reinforce the submarine. You'll need to run more steps of the pair insertion process; a total of 40 steps should do it.
  * 
  * In the above example, the most common element is B (occurring 2192039569602 times) and the least common element is H (occurring 3849876073 times); subtracting these produces 2188189693529.
  * 
  * Apply 40 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?
  * 
  * Your puzzle answer was 3015383850689.
  * 
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2114 extends Utils:
  
  val polymer = "FNFPPNKPPHSOKFFHOFOC"
  val rules = Map(
    "VS" -> 'B',
    "SV" -> 'C',
    "PP" -> 'N',
    "NS" -> 'N',
    "BC" -> 'N',
    "PB" -> 'F',
    "BK" -> 'P',
    "NV" -> 'V',
    "KF" -> 'C',
    "KS" -> 'C',
    "PV" -> 'N',
    "NF" -> 'S',
    "PK" -> 'F',
    "SC" -> 'F',
    "KN" -> 'K',
    "PN" -> 'K',
    "OH" -> 'F',
    "PS" -> 'P',
    "FN" -> 'O',
    "OP" -> 'B',
    "FO" -> 'C',
    "HS" -> 'F',
    "VO" -> 'C',
    "OS" -> 'B',
    "PF" -> 'V',
    "SB" -> 'V',
    "KO" -> 'O',
    "SK" -> 'N',
    "KB" -> 'F',
    "KH" -> 'C',
    "CC" -> 'B',
    "CS" -> 'C',
    "OF" -> 'C',
    "FS" -> 'B',
    "FP" -> 'H',
    "VN" -> 'O',
    "NB" -> 'N',
    "BS" -> 'H',
    "PC" -> 'H',
    "OO" -> 'F',
    "BF" -> 'O',
    "HC" -> 'P',
    "BH" -> 'S',
    "NP" -> 'P',
    "FB" -> 'C',
    "CB" -> 'H',
    "BO" -> 'C',
    "NN" -> 'V',
    "SF" -> 'N',
    "FC" -> 'F',
    "KK" -> 'C',
    "CN" -> 'N',
    "BV" -> 'F',
    "FK" -> 'C',
    "CF" -> 'F',
    "VV" -> 'B',
    "VF" -> 'S',
    "CK" -> 'C',
    "OV" -> 'P',
    "NC" -> 'N',
    "SS" -> 'F',
    "NK" -> 'V',
    "HN" -> 'O',
    "ON" -> 'P',
    "FH" -> 'O',
    "OB" -> 'H',
    "SH" -> 'H',
    "NH" -> 'V',
    "FF" -> 'B',
    "HP" -> 'B',
    "PO" -> 'P',
    "HB" -> 'H',
    "CH" -> 'N',
    "SN" -> 'P',
    "HK" -> 'P',
    "FV" -> 'H',
    "SO" -> 'O',
    "VH" -> 'V',
    "BP" -> 'V',
    "CV" -> 'P',
    "KP" -> 'K',
    "VB" -> 'N',
    "HV" -> 'K',
    "SP" -> 'N',
    "HO" -> 'P',
    "CP" -> 'H',
    "VC" -> 'N',
    "CO" -> 'S',
    "BN" -> 'H',
    "NO" -> 'B',
    "HF" -> 'O',
    "VP" -> 'K',
    "KV" -> 'H',
    "KC" -> 'F',
    "HH" -> 'C',
    "BB" -> 'K',
    "VK" -> 'P',
    "OK" -> 'C',
    "OC" -> 'C',
    "PH" -> 'H',
  )
  val input = readThisObjectInput

  def process(polymer: String): String = 
    (polymer.sliding(2).map( pair => 
      val c = rules(pair)
      val c0 = pair(0)
      String(Array(c0, c))
    ) ++ Seq(polymer.charAt(polymer.length - 1))
    ).mkString
  lazy val answer1: Int =
    val n = 10L
    val res = NumberSequenceUtils.unfoldN(process)(polymer, n)
    val counts = res.toCharArray.toArray.groupMapReduce(identity)(_ => 1)(_ + _).toList
    val mostFrequent = counts.maxBy(_._2)
    val leastFrequent = counts.minBy(_._2)
    mostFrequent._2 - leastFrequent._2

  //Part 2 
  type PairCounts = List[((Char, Char), Long)]

  val rules2 = rules.map{ case (s,ch) => 
    val a = s.charAt(0)
    val b = s.charAt(1)
    (a,b) -> Seq((a, ch), (ch, b)) //(a, ch), (c, b)
  }
  def convert(polymer: String): PairCounts =
    polymer.toCharArray.sliding(2).map{ 
      case Array(a,b) => 
        a -> b
    }.toList
    .groupMapReduce(identity)(_ => 1L)(_ + _)
    .toList

  def process2(polymer: PairCounts): PairCounts = 
    polymer.flatMap{ case (p@(a,b), cnt) => 
      val addPairs = rules2(p)
      addPairs.map(p => (p, cnt))
    }
    .groupMapReduce(_._1)(_._2)(_ + _)
    .toList
  // 1018652774046 6030767701377
  lazy val answer2: Long =
    val n = 40L
    val initial = convert(polymer)
    val res = NumberSequenceUtils.unfoldN(process2)( initial, n)
    val charCounts = (
      (polymer.charAt(0) -> 1L) +: 
      res.map{ case ((_,b), cnt) => (b, cnt) })
    val counts = charCounts
      .groupMapReduce(_._1)(_._2)(_ + _).toList
    val mostFrequent = counts.maxBy(_._2)
    val leastFrequent = counts.minBy(_._2)
    mostFrequent._2 - leastFrequent._2

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
