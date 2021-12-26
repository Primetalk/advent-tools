package org.primetalk.advent2021

import org.primetalk.advent3.tools.Utils
import org.primetalk.advent3.tools.Display2D
import org.primetalk.advent3.tools.NumberSequenceUtils

/**
  * https://adventofcode.com/2021/day/25
  */
object Day2125mutable extends Utils:

  val input = readThisObjectInputLines

  val initialDisplay = Display2D.readCharDisplay(input)
  val buffer = initialDisplay.deepClone

  // def ruleRight
  def moveRight(d1: Display2D[Char], d2: Display2D[Char]): Int = d2.fillAndCountChanges{ p =>
    d1(p) match
      case '.' =>
        val leftPosition = (if p._1 == d1.minX then d1.maxX else p._1 - 1, p._2)  
        if d1(leftPosition) == '>' then
          '>'
        else
          '.'
      case '>' =>
        val rightPosition = (if p._1 == d1.maxX then d1.minX else p._1 + 1, p._2)  
        if d1(rightPosition) == '.' then
          '.'
        else
          '>'
      case v => v
  }

  def moveDown(d1: Display2D[Char], d2: Display2D[Char]): Int = 
    d2.fillAndCountChanges{ p =>
    d1(p) match
    case '.' =>
      val upPosition = (p._1, if p._2 == d1.minY then d1.maxY else p._2 - 1)  
      if d1(upPosition) == 'v' then
        'v'
      else
        '.'
    case 'v' =>
      val downPosition = (p._1, if p._2 == d1.maxY then d1.minY else p._2 + 1)  
      if d1(downPosition) == '.' then
        '.'
      else
        'v'
    case v => v
  }
  def step(d: Display2D[Char], buffer: Display2D[Char]): Int = 
    moveRight(d, buffer) +
      moveDown(buffer, d)
  // 457, 458
  lazy val answer1: Int =
    def loop(cnt: Int = 0): Int =
      val res = step(initialDisplay, buffer)
      if res == 0 then 
        cnt
      else
        loop(cnt + 1)
    
    val res = loop(0)
    res + 1

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
