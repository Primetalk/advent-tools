package org.primetalk.advent2019

import org.primetalk.advent.tools.Complex

/**
  * https://adventofcode.com/2019/day/16
  *
  * --- Day 16: Flawed Frequency Transmission ---
  *
  * You're 3/4ths of the way through the gas giants. Not only do roundtrip signals to Earth take five hours, but the signal quality is quite bad as well. You can clean up the signal with the Flawed Frequency Transmission algorithm, or FFT.
  *
  * As input, FFT takes a list of numbers. In the signal you received (your puzzle input), each number is a single digit: data like 15243 represents the sequence 1, 5, 2, 4, 3.
  *
  * FFT operates in repeated phases. In each phase, a new list is constructed with the same length as the input list. This new list is also used as the input for the next phase.
  *
  * Each element in the new list is built by multiplying every value in the input list by a value in a repeating pattern and then adding up the results. So, if the input list were 9, 8, 7, 6, 5 and the pattern for a given element were 1, 2, 3, the result would be 9*1 + 8*2 + 7*3 + 6*1 + 5*2 (with each input element on the left and each value in the repeating pattern on the right of each multiplication). Then, only the ones digit is kept: 38 becomes 8, -17 becomes 7, and so on.
  *
  * While each element in the output array uses all of the same input array elements, the actual repeating pattern to use depends on which output element is being calculated. The base pattern is 0, 1, 0, -1. Then, repeat each value in the pattern a number of times equal to the position in the output list being considered. Repeat once for the first element, twice for the second element, three times for the third element, and so on. So, if the third element of the output list is being calculated, repeating the values would produce: 0, 0, 0, 1, 1, 1, 0, 0, 0, -1, -1, -1.
  *
  * When applying the pattern, skip the very first value exactly once. (In other words, offset the whole pattern left by one.) So, for the second element of the output list, the actual pattern used would be: 0, 1, 1, 0, 0, -1, -1, 0, 0, 1, 1, 0, 0, -1, -1, ....
  *
  * After using this process to calculate each element of the output list, the phase is complete, and the output list of this phase is used as the new input list for the next phase, if any.
  *
  * Given the input signal 12345678, below are four phases of FFT. Within each phase, each output digit is calculated on a single line with the result at the far right; each multiplication operation shows the input digit on the left and the pattern value on the right:
  *
  * Input signal: 12345678
  *
  * 1*1  + 2*0  + 3*-1 + 4*0  + 5*1  + 6*0  + 7*-1 + 8*0  = 4
  * 1*0  + 2*1  + 3*1  + 4*0  + 5*0  + 6*-1 + 7*-1 + 8*0  = 8
  * 1*0  + 2*0  + 3*1  + 4*1  + 5*1  + 6*0  + 7*0  + 8*0  = 2
  * 1*0  + 2*0  + 3*0  + 4*1  + 5*1  + 6*1  + 7*1  + 8*0  = 2
  * 1*0  + 2*0  + 3*0  + 4*0  + 5*1  + 6*1  + 7*1  + 8*1  = 6
  * 1*0  + 2*0  + 3*0  + 4*0  + 5*0  + 6*1  + 7*1  + 8*1  = 1
  * 1*0  + 2*0  + 3*0  + 4*0  + 5*0  + 6*0  + 7*1  + 8*1  = 5
  * 1*0  + 2*0  + 3*0  + 4*0  + 5*0  + 6*0  + 7*0  + 8*1  = 8
  *
  * After 1 phase: 48226158
  *
  * 4*1  + 8*0  + 2*-1 + 2*0  + 6*1  + 1*0  + 5*-1 + 8*0  = 3
  * 4*0  + 8*1  + 2*1  + 2*0  + 6*0  + 1*-1 + 5*-1 + 8*0  = 4
  * 4*0  + 8*0  + 2*1  + 2*1  + 6*1  + 1*0  + 5*0  + 8*0  = 0
  * 4*0  + 8*0  + 2*0  + 2*1  + 6*1  + 1*1  + 5*1  + 8*0  = 4
  * 4*0  + 8*0  + 2*0  + 2*0  + 6*1  + 1*1  + 5*1  + 8*1  = 0
  * 4*0  + 8*0  + 2*0  + 2*0  + 6*0  + 1*1  + 5*1  + 8*1  = 4
  * 4*0  + 8*0  + 2*0  + 2*0  + 6*0  + 1*0  + 5*1  + 8*1  = 3
  * 4*0  + 8*0  + 2*0  + 2*0  + 6*0  + 1*0  + 5*0  + 8*1  = 8
  *
  * After 2 phases: 34040438
  *
  * 3*1  + 4*0  + 0*-1 + 4*0  + 0*1  + 4*0  + 3*-1 + 8*0  = 0
  * 3*0  + 4*1  + 0*1  + 4*0  + 0*0  + 4*-1 + 3*-1 + 8*0  = 3
  * 3*0  + 4*0  + 0*1  + 4*1  + 0*1  + 4*0  + 3*0  + 8*0  = 4
  * 3*0  + 4*0  + 0*0  + 4*1  + 0*1  + 4*1  + 3*1  + 8*0  = 1
  * 3*0  + 4*0  + 0*0  + 4*0  + 0*1  + 4*1  + 3*1  + 8*1  = 5
  * 3*0  + 4*0  + 0*0  + 4*0  + 0*0  + 4*1  + 3*1  + 8*1  = 5
  * 3*0  + 4*0  + 0*0  + 4*0  + 0*0  + 4*0  + 3*1  + 8*1  = 1
  * 3*0  + 4*0  + 0*0  + 4*0  + 0*0  + 4*0  + 3*0  + 8*1  = 8
  *
  * After 3 phases: 03415518
  *
  * 0*1  + 3*0  + 4*-1 + 1*0  + 5*1  + 5*0  + 1*-1 + 8*0  = 0
  * 0*0  + 3*1  + 4*1  + 1*0  + 5*0  + 5*-1 + 1*-1 + 8*0  = 1
  * 0*0  + 3*0  + 4*1  + 1*1  + 5*1  + 5*0  + 1*0  + 8*0  = 0
  * 0*0  + 3*0  + 4*0  + 1*1  + 5*1  + 5*1  + 1*1  + 8*0  = 2
  * 0*0  + 3*0  + 4*0  + 1*0  + 5*1  + 5*1  + 1*1  + 8*1  = 9
  * 0*0  + 3*0  + 4*0  + 1*0  + 5*0  + 5*1  + 1*1  + 8*1  = 4
  * 0*0  + 3*0  + 4*0  + 1*0  + 5*0  + 5*0  + 1*1  + 8*1  = 9
  * 0*0  + 3*0  + 4*0  + 1*0  + 5*0  + 5*0  + 1*0  + 8*1  = 8
  *
  * After 4 phases: 01029498
  *
  * Here are the first eight digits of the final output list after 100 phases for some larger inputs:
  *
  * 80871224585914546619083218645595 becomes 24176176.
  * 19617804207202209144916044189917 becomes 73745418.
  * 69317163492948606335995924319873 becomes 52432133.
  *
  * After 100 phases of FFT, what are the first eight digits in the final output list?
  *
  * Your puzzle answer was 63794407.
  * --- Part Two ---
  *
  * Now that your FFT is working, you can decode the real signal.
  *
  * The real signal is your puzzle input repeated 10000 times. Treat this new signal as a single input list. Patterns are still calculated as before, and 100 phases of FFT are still applied.
  *
  * The first seven digits of your initial input signal also represent the message offset. The message offset is the location of the eight-digit message in the final output list. Specifically, the message offset indicates the number of digits to skip before reading the eight-digit message. For example, if the first seven digits of your initial input signal were 1234567, the eight-digit message would be the eight digits after skipping 1,234,567 digits of the final output list. Or, if the message offset were 7 and your final output list were 98765432109876543210, the eight-digit message would be 21098765. (Of course, your real message offset will be a seven-digit number, not a one-digit number like 7.)
  *
  * Here is the eight-digit message in the final output list after 100 phases. The message offset given in each input has been highlighted. (Note that the inputs given below are repeated 10000 times to find the actual starting input lists.)
  *
  * 03036732577212944063491565474664 becomes 84462026.
  * 02935109699940807407585447034323 becomes 78725270.
  * 03081770884921959731165446850517 becomes 53553731.
  *
  * After repeating your input signal 10000 times and running 100 phases of FFT, what is the eight-digit message embedded in the final output list?
  *
  * Your puzzle answer was 77247538.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day16 {
  // 6500000 -       5971751  = 28487
  val inputString = "59717513948900379305109702352254961099291386881456676203556183151524797037683068791860532352118123252250974130706958763348105389034831381607519427872819735052750376719383812473081415096360867340158428371353702640632449827967163188043812193288449328058464005995046093112575926165337330100634707115160053682715014464686531460025493602539343245166620098362467196933484413717749680188294435582266877493265037758875197256932099061961217414581388227153472347319505899534413848174322474743198535953826086266146686256066319093589456135923631361106367290236939056758783671975582829257390514211329195992209734175732361974503874578275698611819911236908050184158"
  // len = 650 = 13*5*5*2
  val input: Array[Int] = parse(inputString)
  val basePattern: Seq[Int] = Seq(0,1,0,-1)
  val offset = 1

  def parse(i: String): Array[Int] =
    i.toCharArray.map(c => c.toInt - '0')
  @inline
  def pattern(position: Int)(i: Int): Int = {
    val len1 = position + 1
    val index = (i / len1 ) % 4 // 0 .. 3
    (index % 2) * (2 - index)
  }

  def phase(input: Array[Int]): Array[Int] = {
    val size = input.length
    def digit(position: Int): Int = {
      @scala.annotation.tailrec
      def loop(i: Int, sum: Int): Int = {
        if(i < size)
          loop(i + 1, sum + input(i) * pattern(position)(i + offset))
        else
          sum
      }
      val sum = loop(0, 0)
      math.abs(sum) % 10
    }
    val res = new Array[Int](size)
    @scala.annotation.tailrec
    def pLoop(position: Int): Unit = {
      if(position < size) {
        res(position) = digit(position)
        pLoop(position + 1)
      }
    }
    pLoop(0)
    res
  }

  @scala.annotation.tailrec
  def runN(n: Int, ph0: Array[Int]): Array[Int] = {
    if(n == 0)
      ph0
    else
      runN(n - 1, phase(ph0))
  }

  lazy val answer1: String = {
    val res = runN(100, input)
    res.take(8).mkString("")
  }

  // Part 2

  @inline
  def angle(position: Int)(i: Int): Complex = {
    val len1 = position + 1
    Complex.angle90power(i / len1)
  }

  /**
    * phase2_pos = sum_{i=0}^{L*N - 1} input(i) * (1/4)^{(i+offset)/(pos+1)}
    * Transform sum like in FFT.
    * phase2_pos = sum_{p=0}^{L - 1} sum_{q=0}^{N - 1} input(p + q*L) * (1/4){(p + q*L+offset)/(pos+1)}
    * phase2_pos = sum_{q=0}^{N - 1} sum_{p=0}^{L - 1} input(p + q*L) * (1/4){(p + q*L+offset)/(pos+1)}
    * phase2_pos = sum_{q=0}^{N - 1} sum_{p=0}^{L - 1} input(p + q*L) * (1/4){(p+offset)/(pos+1)} * (1/4){(q*L)/(pos+1)}
    * phase2_pos = sum_{q=0}^{N - 1} (1/4){(q*L)/(pos+1)} * sum_{p=0}^{L - 1} input(p + q*L) * (1/4){(p+offset)/(pos+1)}
    * f
    * f
    * f
    * f
    * -- input(p + q*L) = input(p) // as we repeat the original array N times
    * --- = sum_{p=0}^{L - 1} sum_{q=0}^{N - 1} input(p) * (1/4){(p)/(pos+1)} * (1/4){(q*L+offset)/(pos+1)}
    * -- = sum_{p=0}^{L - 1} input(p) * (1/4){(p)/(pos+1)} * sum_{q=0}^{N - 1} (1/4){(q*L+offset)/(pos+1)}
    * -- We can calculate
    * -- sum_{q=0}^{N - 1} (1/4){(q*L+offset)/(pos+1)}            // ^
    * -- only once.
    */

  def phase2(input: Array[Int], sizeMul: Int): Array[Int] = {
    val size = input.length

    def digit(position: Int): Int = {
      @scala.annotation.tailrec
      def loop(i: Int, sum: Int): Int = {
        if(i < size * sizeMul)
          loop(i + 1, sum + input(i % size) * pattern(position)(i + offset))
        else
          sum
      }
      val sum = loop(0, 0)
      math.abs(sum) % 10
    }
    val res = new Array[Int](size)
    @scala.annotation.tailrec
    def pLoop(position: Int): Unit = {
      if(position < size) {
        res(position) = digit(position)
        pLoop(position + 1)
      }
    }
    pLoop(0)
    res
  }

  def phase3(input: Array[Int], start: Int): Array[Int] = {
    val size = input.length

    val res = new Array[Int](size)
    res(size - 1) = input(size - 1)
    @scala.annotation.tailrec
    def pLoop(position: Int): Unit = {
      if(position >= start) {
        res(position) = (res(position + 1) + input(position)) % 10
        pLoop(position - 1)
      }
    }
    pLoop(size - 2)
    res
  }

  @scala.annotation.tailrec
  def runN2(n: Int, ph0: Array[Int], start: Int): Array[Int] = {
    if(n == 0)
      ph0
    else
      runN2(n - 1, phase3(ph0, start), start)
  }

  lazy val answer2: String = {
    val start = inputString.take(7).toInt
    val fullInput: Array[Int] = (0 until 10000).toArray.flatMap(_ => input)
    val output = runN2(100, fullInput, start)
    val res = (start until (start + 8)).map(output)
    res.mkString("")
  }


  def main(args: Array[String]): Unit = {
//    println("Answer1: " + answer1)
    println("Answer2: " + answer2) // 14305255 // 77247538
  }
}
