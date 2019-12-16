package org.primetalk.advent2019

import org.primetalk.advent.tools.Complex

/**
  * https://adventofcode.com/2019/day/1
  */
object Day16 {
// 6500000 - 5971751 28487
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
